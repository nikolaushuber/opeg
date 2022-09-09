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

let gen_expect_function tok = 
  let (name, _, _) = tok in 
  "let expect_" ^ name ^ " (tknz : tokenizer) = 
  let p_tok = peek_token tknz in 
  match p_tok with 
  | " ^ name ^ " x -> 
    (
      let _ = get_token tknz in 
      x 
    )
  | _ -> raise No_parse 
  "

let gen_expect_functions (g : Grammar.t) = 
  let toks = g.tokens in 
  let toks_with_types = List.filter (fun (_, t, _) -> Option.is_some t) toks in 
  String.concat "\n" (List.map gen_expect_function toks_with_types)
  
let gen_token_type (g : Grammar.t) = 
  let token_to_string tok = 
    let (name, _type, short) = tok in 
    name ^ 
    (match _type with 
    | Some t -> " of " ^ t 
    | None -> "" 
    )
    ^ 
    (
    match short with 
    | Some s -> " (* \"" ^ s ^ "\" *)"
    | None -> ""   
    )
  in 

  let token_list = g.tokens in 
  "type token =\n\t| " ^
  String.concat "\n\t| " (List.map token_to_string token_list) 

let gen_hash_tables (g : Grammar.t) = 
  let gen_hash_table (r : Grammar.rule) = 
    let (name, _type, _) = r in 
    "let " ^ name ^ "_tbl : (int, (" ^ _type ^ " * int) option) Hashtbl.t = Hashtbl.create 100" 
  in 

  let rules = g.rules in 
  let rules_without_start = List.filter (fun (name, _, _) -> name <> g.start_deriv) rules in 
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
  (* This links shortcuts to actual token names *)
  let tok_short_tbl = 
    let tbl = Hashtbl.create (List.length g.tokens) in 
    List.iter 
      (
        fun (name, _, short) -> 
          match short with 
          | Some s -> Hashtbl.add tbl s name 
          | None -> Hashtbl.add tbl name name 
      ) 
    g.tokens; 
    tbl 
  in

  let gen_symbol (s : Grammar.symbol) = match s with 
    | Nonterminal (name, syn) -> 
      "let " ^ syn ^ " = " ^ name ^ " tknz in" 
    | Terminal (name, Some syn) -> 
      "let " ^ syn ^ " = expect_" ^ name ^ " tknz in" 
    | Terminal (name, None) -> 
      try 
        "let _ = expect tknz " ^ Hashtbl.find tok_short_tbl name ^ " in" 
      with 
        Not_found -> failwith ("Unknown terminal symbol: '" ^ name ^ "'.")
  in 

  let gen_alt (d : Grammar.deriv) (idx : int) (num_alt : int) = 
    let (sym_l, act) = d in 
"\t(* / " ^ String.concat " " (List.map Grammar.pp_symbol sym_l) ^ " { ... } *)\n" ^  
"\tlet alt" ^ string_of_int idx ^ " tknz pos = 
  try
    " ^ 
    (String.concat "\n\t\t" (List.map gen_symbol sym_l)) ^ 
    "\n\t\t(" ^ act ^ ")    
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

  let gen_rule (r : Grammar.rule) = 
    let (name, _, deriv_l) = r in 
    let num_alt = List.length deriv_l in 
"
and " ^ name ^ " tknz = 
" ^ String.concat "\n" (List.rev (List.mapi (fun i d -> gen_alt d (i+1) num_alt) deriv_l))
^ 
"\n\tlookup_or_compute tknz " ^ name ^ "_tbl alt1" 
  in 

  let gen_start (r : Grammar.rule) = 
    let (_, _, deriv_l) = r in 
    let num_alt = List.length deriv_l in 
"let rec start tknz =\n" ^ String.concat "\n" (List.rev (List.mapi (fun i d -> gen_alt d (i+1) num_alt) deriv_l))
^ 
"\n\tlet pos = mark tknz in\n\talt1 tknz pos\n"
  in 

  try   
    let start_rule = List.find (fun (name, _, _) -> String.equal name g.start_deriv) g.rules in 
    let rules_without_start = List.filter (fun (name, _, _) -> name <> g.start_deriv) g.rules in 
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
