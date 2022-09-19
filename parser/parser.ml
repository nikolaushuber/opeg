open OpegLib

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
	| TOK_OPTION (* "?" *)
	| TOK_STAR (* "*" *)
	| TOK_PLUS (* "+" *)
	| TOK_SEC_DIVIDE (* "%%" *)
	| TOK_EOF (* "eof" *)

let expect_TOK_HEADER tknz = 
  let p_tok = peek_token tknz in 
  match p_tok with 
  | TOK_HEADER x -> 
    (
      let _ = get_token tknz in 
      Parse x 
    )
  | _ -> No_parse 
  
let expect_TOK_SEM_ACTION tknz = 
  let p_tok = peek_token tknz in 
  match p_tok with 
  | TOK_SEM_ACTION x -> 
    (
      let _ = get_token tknz in 
      Parse x 
    )
  | _ -> No_parse 
  
let expect_TOK_NAME tknz = 
  let p_tok = peek_token tknz in 
  match p_tok with 
  | TOK_NAME x -> 
    (
      let _ = get_token tknz in 
      Parse x 
    )
  | _ -> No_parse 
  
let expect_TOK_TYPE tknz = 
  let p_tok = peek_token tknz in 
  match p_tok with 
  | TOK_TYPE x -> 
    (
      let _ = get_token tknz in 
      Parse x 
    )
  | _ -> No_parse 
  
let tok_tbl : (int, (Parsetree.pt_tok * int) option) Hashtbl.t = Hashtbl.create 100
let rule_tbl : (int, (Parsetree.pt_rule * int) option) Hashtbl.t = Hashtbl.create 100
let alt_tbl : (int, (Parsetree.pt_alt * int) option) Hashtbl.t = Hashtbl.create 100
let symbol_tbl : (int, (Parsetree.pt_symb * int) option) Hashtbl.t = Hashtbl.create 100
let suffix_tbl : (int, (Parsetree.symb_suffix * int) option) Hashtbl.t = Hashtbl.create 100

let rec start tknz =
	let pos = mark tknz in

	(* / hd = "TOK_HEADER"? "token" ":" tl = tok+ "parser" name = "TOK_NAME" start_deriv = "TOK_NAME" "%%" rl = rule+ "eof" { ... } *)
	let alt1 tknz = 
    let res =
      let* hd = optional ( expect_TOK_HEADER tknz ) in
			let* _ = expect tknz TOK_KW_TOKEN in
			let* _ = expect tknz TOK_DOUBLE_DOTS in
			let* tl = nonempty_nonterminal_list tok tknz in
			let* _ = expect tknz TOK_KW_PARSER in
			let* name = expect_TOK_NAME tknz in
			let* start_deriv = expect_TOK_NAME tknz in
			let* _ = expect tknz TOK_SEC_DIVIDE in
			let* rl = nonempty_nonterminal_list rule tknz in
			let* _ = expect tknz TOK_EOF in
			Parse ((
            {
                header = hd; 
                parser_name = name;
                start_deriv = start_deriv; 
                tokens = tl; 
                rules = rl;
            } 
        ) : Parsetree.t)   
    in
      return res tknz pos
  in

	alt1 tknz

and tok tknz = 
  let pos = mark tknz in 

  	(* / "|" n = "TOK_NAME" t = "TOK_TYPE"? short = "TOK_NAME"? { ... } *)
	let alt1 tknz = 
    let res =
      let* _ = expect tknz TOK_BAR in
			let* n = expect_TOK_NAME tknz in
			let* t = optional ( expect_TOK_TYPE tknz ) in
			let* short = optional ( expect_TOK_NAME tknz ) in
			Parse (( (n, t, short) ) : Parsetree.pt_tok)   
    in
      return res tknz pos
  in

	lookup_or_compute tknz tok_tbl alt1


and rule tknz = 
  let pos = mark tknz in 

  	(* / n = "TOK_NAME" t = "TOK_TYPE" ":" dl = alt+ { ... } *)
	let alt1 tknz = 
    let res =
      let* n = expect_TOK_NAME tknz in
			let* t = expect_TOK_TYPE tknz in
			let* _ = expect tknz TOK_DOUBLE_DOTS in
			let* dl = nonempty_nonterminal_list alt tknz in
			Parse (( (n, t, dl) ) : Parsetree.pt_rule)   
    in
      return res tknz pos
  in

	lookup_or_compute tknz rule_tbl alt1


and alt tknz = 
  let pos = mark tknz in 

  	(* / "/" sl = symbol* act = "TOK_SEM_ACTION" { ... } *)
	let alt1 tknz = 
    let res =
      let* _ = expect tknz TOK_CHOICE in
			let* sl = nonterminal_list symbol tknz in
			let* act = expect_TOK_SEM_ACTION tknz in
			Parse (( (sl, act) ) : Parsetree.pt_alt)   
    in
      return res tknz pos
  in

	lookup_or_compute tknz alt_tbl alt1


and symbol tknz = 
  let pos = mark tknz in 

  	(* / n = "TOK_NAME" suff = suffix? { ... } *)
	let alt2 tknz = 
    let res =
      let* n = expect_TOK_NAME tknz in
			let* suff = optional ( suffix tknz ) in
			Parse (( (n, None, suff) ) : Parsetree.pt_symb)   
    in
      return res tknz pos
  in

	(* / syn = "TOK_NAME" "=" n = "TOK_NAME" suff = suffix? { ... } *)
	let alt1 tknz = 
    let res =
      let* syn = expect_TOK_NAME tknz in
			let* _ = expect tknz TOK_EQUALS in
			let* n = expect_TOK_NAME tknz in
			let* suff = optional ( suffix tknz ) in
			Parse (( (n, Some syn, suff) ) : Parsetree.pt_symb)   
    in 
      return_or_try_next res tknz pos alt2
  in

	lookup_or_compute tknz symbol_tbl alt1


and suffix tknz = 
  let pos = mark tknz in 

  	(* / "*" { ... } *)
	let alt3 tknz = 
    let res =
      let* _ = expect tknz TOK_STAR in
			Parse (( Star ) : Parsetree.symb_suffix)   
    in
      return res tknz pos
  in

	(* / "+" { ... } *)
	let alt2 tknz = 
    let res =
      let* _ = expect tknz TOK_PLUS in
			Parse (( Plus ) : Parsetree.symb_suffix)   
    in 
      return_or_try_next res tknz pos alt3
  in

	(* / "?" { ... } *)
	let alt1 tknz = 
    let res =
      let* _ = expect tknz TOK_OPTION in
			Parse (( Optional ) : Parsetree.symb_suffix)   
    in 
      return_or_try_next res tknz pos alt2
  in

	lookup_or_compute tknz suffix_tbl alt1

let parse lexer lexbuf = 
  let tknz = make_tokenizer lexbuf lexer in
  Hashtbl.reset tok_tbl;
Hashtbl.reset rule_tbl;
Hashtbl.reset alt_tbl;
Hashtbl.reset symbol_tbl;
Hashtbl.reset suffix_tbl;  
  let res = start tknz in 
  match res with 
  | Parse x -> x 
  | No_parse -> raise Parse_error 
