(* open Grammar 

let gen_header (g : Grammar.t) = 
  (if 
    Option.is_some g.header 
  then 
    (
      "(* ------ HEADER -------- *)" ^
      Option.get g.header ^
      "(* ------ HEADER END ---- *)\n\n"
    )
  else 
    "" 
  ) ^ "open OpegLib\n\n" 

let gen_expect_function (tok : Grammar.Token.t) = 
  "let expect_" ^ tok.name ^ " tknz = 
  let p_tok = peek_token tknz in 
  match p_tok with 
  | " ^ tok.name ^ " x -> 
    (
      let _ = get_token tknz in 
      Parse x 
    )
  | _ -> No_parse 
  "

let gen_expect_functions (g : Grammar.t) =
  let open Token in  
  let toks = g.tokens in 
  let toks_with_types = List.filter (fun tk -> Option.is_some tk.ty) toks in 
  String.concat "\n" (List.map gen_expect_function toks_with_types) ^ 
  "\n"
  
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
  ^ "\n\n"

let gen_hash_tables (g : Grammar.t) = 
  let open Rule in 
  let gen_hash_table (r : Grammar.Rule.t) = 
    "let " ^ r.name ^ "_tbl : (int, (" ^ r.ty ^ " * int) option) Hashtbl.t = Hashtbl.create 100" 
  in 

  let rules = g.rules in 
  let rules_without_start = List.filter (fun r -> r.name <> g.start_deriv) rules in 
  String.concat "\n" (List.map gen_hash_table rules_without_start) ^ "\n\n"

let gen_hash_resets (g : Grammar.t) = 
  let open Rule in 
  let gen_hash_reset (r : Rule.t) = 
    "Hashtbl.reset " ^ r.name ^ "_tbl;" 
  in 
  let rules_without_start = List.filter (fun r -> r.name <> g.start_deriv) g.rules in 
  String.concat "\n\t" (List.map gen_hash_reset rules_without_start)  

let gen_derivations (g : Grammar.t) = 
  let ginfo = Grammar.Info.get_info g in 

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
    (* Is there a name binding for the parse result? *)
    let lhs = match s.ref with 
      | None -> "let* _ = " 
      | Some s -> "let* " ^ s ^ " = " 
    in 

    (* Construct the actual parse phrase *)
    let parse_phrase = match (s.ref, s.terminal) with 
      | (Some _, true) -> "expect_" ^ s.name ^ " tknz" 
      | (None, true) -> 
        (
          try 
            "expect tknz " ^ Hashtbl.find tok_short_tbl s.name 
          with 
            Not_found -> failwith ("Unknown terminal symbol: '" ^ s.name ^ "'.")
        )
      | (_, false) -> s.name ^ " tknz" 
    in

    lhs ^ 
    match s.suffix with 
    | Empty -> parse_phrase ^ " in" 
    | Optional -> "optional ( " ^ parse_phrase ^ " ) in" 
    | Plus -> (
      (* Is it a list of terminals? *)
      match (s.ref, s.terminal) with 
      | (Some _, true) -> "nonempty_lexeme_list " ^ parse_phrase ^ " in" 
      | (None, true) -> "nonempty_terminal_list " ^ parse_phrase ^ " in" 
      | (_, false) -> "nonempty_nonterminal_list " ^ parse_phrase ^ " in" 
    )
    | Star -> (
      (* Is it a list of terminals? *)
      match (s.ref, s.terminal) with 
      | (Some _, true) -> "lexeme_list " ^ parse_phrase ^ " in" 
      | (None, true) -> "terminal_list " ^ parse_phrase ^ " in" 
      | (_, false) -> "nonterminal_list " ^ parse_phrase ^ " in" 
    )

  in 

  let gen_alt (d : Grammar.Alternative.t) (idx : int) (num_alt : int) (ty : string) = 
    let pp_symbol s = Grammar.Symbol.pp s 
    in 

"\t(* / " ^ String.concat " " (List.map pp_symbol d.symbols) ^ " { ... } *)\n" ^  
"\tlet alt" ^ string_of_int idx ^ " tknz = 
    let res =
      " ^ 
      (String.concat "\n\t\t\t" (List.map gen_symbol d.symbols)) ^ 
      "\n\t\t\tParse ((" ^ d.action ^ ") : " ^ ty ^ ")   
    in" ^ (
      if idx == num_alt then 
      "
      return res tknz pos"   
      else 
      " 
      return_or_try_next res tknz pos alt" ^ string_of_int (idx+1) 
      ) ^ 
"
  in
" 
  in 

  let gen_rule (r : Grammar.Rule.t) = 
    let num_alt = List.length r.alts in 
"
and " ^ r.name ^ " tknz = 
  let pos = mark tknz in 

" ^ String.concat "\n" (List.rev (List.mapi (fun i d -> gen_alt d (i+1) num_alt r.ty) r.alts))
^
let rinfo = List.assoc r.name ginfo in 
match rinfo.left with 
| Direct -> "\n\tdirect_left_recurse tknz " ^ r.name ^ "_tbl alt1" 
| _ ->
  "\n\tlookup_or_compute tknz " ^ r.name ^ "_tbl alt1" 
  in 

  let gen_start (r : Grammar.Rule.t) (_rec : bool) = 
    let num_alt = List.length r.alts in 
"let" ^ (if _rec then " rec " else " ") ^ "start tknz =\n" ^
"\tlet pos = mark tknz in\n\n" ^ String.concat "\n" (List.rev (List.mapi (fun i d -> gen_alt d (i+1) num_alt r.ty) r.alts))
^ 
"\n\talt1 tknz\n"
  in 

  try   
    let start_rule = List.find (fun r -> String.equal r.Rule.name g.start_deriv) g.rules in 
    let rules_without_start = List.filter (fun r -> r.Rule.name <> g.start_deriv) g.rules in 
    gen_start start_rule (List.length g.rules > 1) ^ 
    String.concat "\n\n" (List.map gen_rule rules_without_start) ^ "\n\n"
  with 
    Not_found -> failwith ("Starting rule " ^ g.start_deriv ^ " not defined.")
  
let gen_toplevel_fun (g : Grammar.t) = 
  let name =  g.parser_name in 
"let " ^ name ^ " lexer lexbuf = 
  let tknz = make_tokenizer lexbuf lexer in
  " ^ 
  gen_hash_resets g ^ 
"  
  let res = start tknz in 
  match res with 
  | Parse x -> x 
  | No_parse -> raise Parse_error 
"

let string_of_grammar (grammar : Grammar.t) = 
  (gen_header grammar) ^
  (gen_token_type grammar) ^
  (gen_expect_functions grammar) ^
  (gen_hash_tables grammar) ^
  (gen_derivations grammar) ^
  (gen_toplevel_fun grammar)  *)
