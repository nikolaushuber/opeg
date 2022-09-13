open Grammar 

let gen_header (g : Grammar.t) = 
  if 
    Option.is_some g.header 
  then 
    (
      "(* ------ HEADER -------- *)" ^
      Option.get g.header ^
      "(* ------ HEADER END ---- *)"
    )
  else 
    "" 


let tokenizer_string = 
"exception No_parse 

type tokenizer = {
  lbuf : Lexing.lexbuf; 
  lexer : Lexing.lexbuf -> token; 
  mutable token_list : token list;
  mutable pos: int; 
}

let make_tokenizer lbuf lexer = 
  {
    lbuf = lbuf; 
    lexer = lexer;
    token_list = []; 
    pos = 0; 
  }

let mark (tnz : tokenizer) = 
  tnz.pos

let reset (tnz : tokenizer) (p : int) = 
  tnz.pos <- p 

let peek_token (tnz: tokenizer) = 
  if tnz.pos == List.length tnz.token_list then 
    tnz.token_list <- tnz.token_list @ [tnz.lexer tnz.lbuf]; 
  List.nth tnz.token_list tnz.pos 

let get_token (tnz: tokenizer) = 
  let tok = peek_token tnz in 
  tnz.pos <- tnz.pos + 1; 
  tok

let expect (tnz : tokenizer) (tok : token) = 
  let p_tok = peek_token tnz in  
  if p_tok = tok then 
    get_token tnz
  else 
    raise No_parse 
"

let gen_expect_function (tok : Grammar.Token.t) = 
  "let expect_" ^ tok.name ^ " (tknz : tokenizer) = 
  let p_tok = peek_token tknz in 
  match p_tok with 
  | " ^ tok.name ^ " x -> 
    (
      let _ = get_token tknz in 
      x 
    )
  | _ -> raise No_parse 
  "

let gen_expect_functions (g : Grammar.t) =
  let open Token in  
  let toks = g.tokens in 
  let toks_with_types = List.filter (fun tk -> Option.is_some tk.ty) toks in 
  String.concat "\n" (List.map gen_expect_function toks_with_types)
  
let gen_token_type (g : Grammar.t) = 
  let open Token in 
  let token_to_string tok = 
    tok.name ^ 
    (match tok.ty with 
    | Some t -> " of " ^ t 
    | None -> "" 
    )
    ^ 
    (
    match tok.short with 
    | Some s -> " (* \"" ^ s ^ "\" *)"
    | None -> ""   
    )
  in 

  let token_list = g.tokens in 
  "type token =\n\t| " ^
  String.concat "\n\t| " (List.map token_to_string token_list) 

let gen_hash_tables (g : Grammar.t) = 
  let open Rule in 
  let gen_hash_table (r : Grammar.Rule.t) = 
    "let " ^ r.name ^ "_tbl : (int, (" ^ r.ty ^ " * int) option) Hashtbl.t = Hashtbl.create 100" 
  in 

  let rules = g.rules in 
  let rules_without_start = List.filter (fun r -> r.name <> g.start_deriv) rules in 
  String.concat "\n" (List.map gen_hash_table rules_without_start) 

let lookup_or_compute_string = 
"let lookup_or_compute tknz tbl rule = 
  let pos = mark tknz in 
  if Hashtbl.mem tbl pos then 
    match Hashtbl.find tbl pos with 
    | Some (ast, new_pos) -> (
      reset tknz new_pos; 
      ast) 
    | None -> (
      raise No_parse 
      )
  else 
    try 
      let res = rule tknz pos in 
      let new_pos = mark tknz in 
      Hashtbl.add tbl pos (Some (res, new_pos)); 
      res 
    with 
      No_parse -> 
        (
          Hashtbl.add tbl pos None; 
          raise No_parse 
        )
"

let gen_derivations (g : Grammar.t) = 
  let open Token in 
  (* This links shortcuts to actual token names *)
  let tok_short_tbl = 
    let tbl = Hashtbl.create (List.length g.tokens) in 
    List.iter 
      (
        fun tok -> 
          match tok.short with 
          | Some s -> Hashtbl.add tbl s tok.name 
          | None -> Hashtbl.add tbl tok.name tok.name 
      ) 
    g.tokens; 
    tbl 
  in

  let gen_symbol (s : Grammar.Symbol.t) = 
    let open Symbol in 
    match (s.name, s.ref, s.terminal) with 
    | (name, Some syn, false) -> 
      "let " ^ syn ^ " = " ^ name ^ " tknz in" 
    | (name, None, false) -> 
      "let _ = " ^ name ^ " tknz in" 
    | (name, Some syn, true) -> 
      "let " ^ syn ^ " = expect_" ^ name ^ " tknz in" 
    | (name, None, true) -> 
      try 
        "let _ = expect tknz " ^ Hashtbl.find tok_short_tbl name ^ " in" 
      with 
        Not_found -> failwith ("Unknown terminal symbol: '" ^ name ^ "'.")
  in 

  let gen_alt (d : Grammar.Alternative.t) (idx : int) (num_alt : int) = 
    let pp_symbol _ = ""
    in 

"\t(* / " ^ String.concat " " (List.map pp_symbol d.symbols) ^ " { ... } *)\n" ^  
"\tlet alt" ^ string_of_int idx ^ " tknz pos = 
  try
    " ^ 
    (String.concat "\n\t\t" (List.map gen_symbol d.symbols)) ^ 
    "\n\t\t(" ^ d.action ^ ")    
  with" ^ (
    if idx == num_alt then 
    "
    No_parse -> (reset tknz pos; raise No_parse)"   
    else 
    " 
    No_parse -> (reset tknz pos; alt" ^ string_of_int (idx+1) ^ " tknz pos)"
    ) ^ 
"
  in
" 
  in 

  let gen_rule (r : Grammar.Rule.t) = 
    let num_alt = List.length r.alts in 
"
and " ^ r.name ^ " tknz = 
" ^ String.concat "\n" (List.rev (List.mapi (fun i d -> gen_alt d (i+1) num_alt) r.alts))
^ 
"\n\tlookup_or_compute tknz " ^ r.name ^ "_tbl alt1" 
  in 

  let gen_start (r : Grammar.Rule.t) = 
    let num_alt = List.length r.alts in 
"let rec start tknz =\n" ^ String.concat "\n" (List.rev (List.mapi (fun i d -> gen_alt d (i+1) num_alt) r.alts))
^ 
"\n\tlet pos = mark tknz in\n\talt1 tknz pos\n"
  in 

  try   
    let start_rule = List.find (fun r -> String.equal r.Rule.name g.start_deriv) g.rules in 
    let rules_without_start = List.filter (fun r -> r.Rule.name <> g.start_deriv) g.rules in 
    gen_start start_rule ^ 
    String.concat "\n\n" (List.map gen_rule rules_without_start) 
  with 
    Not_found -> failwith ("Starting rule " ^ g.start_deriv ^ " not defined.")
  
let gen_toplevel_fun (g : Grammar.t) = 
  let name =  g.parser_name in 
"let " ^ name ^ " lexer lexbuf = 
  let tknz = make_tokenizer lexer lexbuf in 
  start tknz 
"
