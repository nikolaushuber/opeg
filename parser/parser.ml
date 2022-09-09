(* ------ HEADER -------- *)
open Grammar 
(* ------ HEADER END ---- *)

type token =
	| TOK_KW_PARSER (* "parser" *)
	| TOK_KW_TOKEN (* "token" *)
	| TOK_HEADER of string
	| TOK_SEM_ACTION of string
	| TOK_NAME of string
	| TOK_TYPE of string
	| TOK_DOUBLE_DOTS (* ":" *)
	| TOK_CHOICE (* "/" *)
	| TOK_BAR (* "|" *)
	| TOK_EQUALS (* "=" *)
	| TOK_SEC_DIVIDE (* "%%" *)
	| TOK_EOF (* "eof" *)

exception No_parse 

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


let expect_TOK_HEADER (tknz : tokenizer) = 
  let p_tok = peek_token tknz in 
  match p_tok with 
  | TOK_HEADER x -> 
    (
      let _ = get_token tknz in 
      x 
    )
  | _ -> raise No_parse 
  
let expect_TOK_SEM_ACTION (tknz : tokenizer) = 
  let p_tok = peek_token tknz in 
  match p_tok with 
  | TOK_SEM_ACTION x -> 
    (
      let _ = get_token tknz in 
      x 
    )
  | _ -> raise No_parse 
  
let expect_TOK_NAME (tknz : tokenizer) = 
  let p_tok = peek_token tknz in 
  match p_tok with 
  | TOK_NAME x -> 
    (
      let _ = get_token tknz in 
      x 
    )
  | _ -> raise No_parse 
  
let expect_TOK_TYPE (tknz : tokenizer) = 
  let p_tok = peek_token tknz in 
  match p_tok with 
  | TOK_TYPE x -> 
    (
      let _ = get_token tknz in 
      x 
    )
  | _ -> raise No_parse 
  

let tokens_tbl : (int, (Grammar.tok list * int) option) Hashtbl.t = Hashtbl.create 100
let tok_tbl : (int, (Grammar.tok * int) option) Hashtbl.t = Hashtbl.create 100
let rules_tbl : (int, (Grammar.rule list * int) option) Hashtbl.t = Hashtbl.create 100
let rule_tbl : (int, (Grammar.rule * int) option) Hashtbl.t = Hashtbl.create 100
let derivs_tbl : (int, (Grammar.deriv list * int) option) Hashtbl.t = Hashtbl.create 100
let deriv_tbl : (int, (Grammar.deriv * int) option) Hashtbl.t = Hashtbl.create 100
let symbols_tbl : (int, (Grammar.symbol list * int) option) Hashtbl.t = Hashtbl.create 100
let symbol_tbl : (int, (Grammar.symbol * int) option) Hashtbl.t = Hashtbl.create 100

let lookup_or_compute tknz tbl rule = 
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


let rec start tknz =
	(* / 'TOK_HEADER'<hd> 'token' ':' tl = tokens 'parser' 'TOK_NAME'<name> 'TOK_NAME'<start_deriv> '%%' rl = rules 'eof' { ... } *)
	let alt1 tknz pos = 
  try
    let hd = expect_TOK_HEADER tknz in
		let _ = expect tknz TOK_KW_TOKEN in
		let _ = expect tknz TOK_DOUBLE_DOTS in
		let tl = tokens tknz in
		let _ = expect tknz TOK_KW_PARSER in
		let name = expect_TOK_NAME tknz in
		let start_deriv = expect_TOK_NAME tknz in
		let _ = expect tknz TOK_SEC_DIVIDE in
		let rl = rules tknz in
		let _ = expect tknz TOK_EOF in
		(
            {
                header = Some hd; 
                parser_name = name;
                start_deriv = start_deriv; 
                tokens = tl; 
                rules = rl;
            }
        )    
  with
    No_parse -> (reset tknz pos; raise No_parse)
  in

	let pos = mark tknz in
	alt1 tknz pos

and tokens tknz = 
	(* / t = tok { ... } *)
	let alt2 tknz pos = 
  try
    let t = tok tknz in
		( [t] )    
  with
    No_parse -> (reset tknz pos; raise No_parse)
  in

	(* / t = tok tl = tokens { ... } *)
	let alt1 tknz pos = 
  try
    let t = tok tknz in
		let tl = tokens tknz in
		( t :: tl )    
  with 
    No_parse -> (reset tknz pos; alt2 tknz pos)
  in

	lookup_or_compute tknz tokens_tbl alt1


and tok tknz = 
	(* / '|' 'TOK_NAME'<n> { ... } *)
	let alt4 tknz pos = 
  try
    let _ = expect tknz TOK_BAR in
		let n = expect_TOK_NAME tknz in
		( (n, None, None) )    
  with
    No_parse -> (reset tknz pos; raise No_parse)
  in

	(* / '|' 'TOK_NAME'<n> 'TOK_NAME'<short> { ... } *)
	let alt3 tknz pos = 
  try
    let _ = expect tknz TOK_BAR in
		let n = expect_TOK_NAME tknz in
		let short = expect_TOK_NAME tknz in
		( (n, None, Some short) )    
  with 
    No_parse -> (reset tknz pos; alt4 tknz pos)
  in

	(* / '|' 'TOK_NAME'<n> 'TOK_TYPE'<t> { ... } *)
	let alt2 tknz pos = 
  try
    let _ = expect tknz TOK_BAR in
		let n = expect_TOK_NAME tknz in
		let t = expect_TOK_TYPE tknz in
		( (n, Some t, None) )    
  with 
    No_parse -> (reset tknz pos; alt3 tknz pos)
  in

	(* / '|' 'TOK_NAME'<n> 'TOK_TYPE'<t> 'TOK_NAME'<short> { ... } *)
	let alt1 tknz pos = 
  try
    let _ = expect tknz TOK_BAR in
		let n = expect_TOK_NAME tknz in
		let t = expect_TOK_TYPE tknz in
		let short = expect_TOK_NAME tknz in
		( (n, Some t, Some short) )    
  with 
    No_parse -> (reset tknz pos; alt2 tknz pos)
  in

	lookup_or_compute tknz tok_tbl alt1


and rules tknz = 
	(* / r = rule { ... } *)
	let alt2 tknz pos = 
  try
    let r = rule tknz in
		( [r] )    
  with
    No_parse -> (reset tknz pos; raise No_parse)
  in

	(* / r = rule rl = rules { ... } *)
	let alt1 tknz pos = 
  try
    let r = rule tknz in
		let rl = rules tknz in
		( r :: rl )    
  with 
    No_parse -> (reset tknz pos; alt2 tknz pos)
  in

	lookup_or_compute tknz rules_tbl alt1


and rule tknz = 
	(* / 'TOK_NAME'<n> 'TOK_TYPE'<t> ':' dl = derivs { ... } *)
	let alt1 tknz pos = 
  try
    let n = expect_TOK_NAME tknz in
		let t = expect_TOK_TYPE tknz in
		let _ = expect tknz TOK_DOUBLE_DOTS in
		let dl = derivs tknz in
		( (n, t, dl) )    
  with
    No_parse -> (reset tknz pos; raise No_parse)
  in

	lookup_or_compute tknz rule_tbl alt1


and derivs tknz = 
	(* / d = deriv { ... } *)
	let alt2 tknz pos = 
  try
    let d = deriv tknz in
		( [d] )    
  with
    No_parse -> (reset tknz pos; raise No_parse)
  in

	(* / d = deriv dl = derivs { ... } *)
	let alt1 tknz pos = 
  try
    let d = deriv tknz in
		let dl = derivs tknz in
		( d :: dl )    
  with 
    No_parse -> (reset tknz pos; alt2 tknz pos)
  in

	lookup_or_compute tknz derivs_tbl alt1


and deriv tknz = 
	(* / '/' sl = symbols 'TOK_SEM_ACTION'<act> { ... } *)
	let alt1 tknz pos = 
  try
    let _ = expect tknz TOK_CHOICE in
		let sl = symbols tknz in
		let act = expect_TOK_SEM_ACTION tknz in
		( (sl, act) )    
  with
    No_parse -> (reset tknz pos; raise No_parse)
  in

	lookup_or_compute tknz deriv_tbl alt1


and symbols tknz = 
	(* / s = symbol { ... } *)
	let alt2 tknz pos = 
  try
    let s = symbol tknz in
		( [s] )    
  with
    No_parse -> (reset tknz pos; raise No_parse)
  in

	(* / s = symbol sl = symbols { ... } *)
	let alt1 tknz pos = 
  try
    let s = symbol tknz in
		let sl = symbols tknz in
		( s :: sl )    
  with 
    No_parse -> (reset tknz pos; alt2 tknz pos)
  in

	lookup_or_compute tknz symbols_tbl alt1


and symbol tknz = 
	(* / 'TOK_NAME'<n> { ... } *)
	let alt3 tknz pos = 
  try
    let n = expect_TOK_NAME tknz in
		( Terminal (n, None) )    
  with
    No_parse -> (reset tknz pos; raise No_parse)
  in

	(* / 'TOK_NAME'<n> 'TOK_TYPE'<lex> { ... } *)
	let alt2 tknz pos = 
  try
    let n = expect_TOK_NAME tknz in
		let lex = expect_TOK_TYPE tknz in
		( Terminal (n, Some lex) )    
  with 
    No_parse -> (reset tknz pos; alt3 tknz pos)
  in

	(* / 'TOK_NAME'<syn> '=' 'TOK_NAME'<n> { ... } *)
	let alt1 tknz pos = 
  try
    let syn = expect_TOK_NAME tknz in
		let _ = expect tknz TOK_EQUALS in
		let n = expect_TOK_NAME tknz in
		( Nonterminal (n, syn) )    
  with 
    No_parse -> (reset tknz pos; alt2 tknz pos)
  in

	lookup_or_compute tknz symbol_tbl alt1

let parse lexer lexbuf = 
  let tknz = make_tokenizer lexbuf lexer in 
  start tknz 


