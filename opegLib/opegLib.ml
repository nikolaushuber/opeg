exception Parse_error 

type 'a parse_result = 
| No_parse 
| Parse of 'a 

type 'a tokenizer = {
  lbuf : Lexing.lexbuf; 
  lexer : Lexing.lexbuf -> 'a; 
  mutable token_list : 'a list;
  mutable pos: int; 
}

let make_tokenizer lbuf lexer = 
{
  lbuf = lbuf; 
  lexer = lexer;
  token_list = []; 
  pos = 0; 
}

let ( let* ) o f = match o with 
  | No_parse -> No_parse 
  | Parse x -> f x 

let mark (tnz) = 
  tnz.pos

let reset (tnz) (p : int) = 
tnz.pos <- p 

let peek_token (tnz) = 
if tnz.pos == List.length tnz.token_list then 
  tnz.token_list <- tnz.token_list @ [tnz.lexer tnz.lbuf]; 
List.nth tnz.token_list tnz.pos 

let get_token (tnz) = 
let tok = peek_token tnz in 
tnz.pos <- tnz.pos + 1; 
tok

let expect (tnz) (tok) = 
  let p_tok = peek_token tnz in  
  if p_tok = tok then 
    Parse (get_token tnz)
  else 
    No_parse 

let return res tknz pos = match res with 
  | Parse x -> Parse x 
  | No_parse -> (reset tknz pos; No_parse) 

let return_or_try_next res tknz pos next = match res with 
  | Parse x -> Parse x 
  | No_parse -> (reset tknz pos; next tknz)

let optional res = match res with 
  | Parse x -> Parse (Some x)
  | No_parse -> Parse (None)


let lookup_or_compute tknz tbl rule = 
  let pos = mark tknz in 
  if Hashtbl.mem tbl pos then 
    match Hashtbl.find tbl pos with 
    | Some (ast, new_pos) -> (
      reset tknz new_pos; 
      Parse ast) 
    | None -> No_parse
  else 
    let res = rule tknz in 
    match res with 
    | Parse x -> 
      (
        let new_pos = mark tknz in 
        Hashtbl.add tbl pos (Some (x, new_pos)); 
        res 
      )
    | No_parse -> 
      (
        Hashtbl.add tbl pos None; 
        No_parse 
      )
        
let nonterminal_list rule tknz = 
  let rec _inner rule tknz acc = 
    let pos = mark tknz in 
    let res = rule tknz in 
    match res with 
    | Parse x -> _inner rule tknz (x :: acc) 
    | No_parse -> (reset tknz pos; Parse (List.rev acc))
  in 
  _inner rule tknz [] 

let nonempty_nonterminal_list rule tknz = 
  let pos = mark tknz in 
  let res = nonterminal_list rule tknz in 
  match res with 
  | Parse xl -> if List.length xl > 0 then res else (reset tknz pos; No_parse)
  | No_parse -> (failwith "nonterminal_list should never return No_parse")

let terminal_list expect tknz tok = 
  let rec _inner expect tknz tok acc = 
    let pos = mark tknz in 
    let res = expect tknz tok in 
    match res with 
    | Parse x -> _inner expect tknz tok (x :: acc) 
    | No_parse -> (reset tknz pos; Parse (List.rev acc)) 
  in 
  _inner expect tknz tok [] 

let nonempty_terminal_list expect tknz tok = 
  let pos = mark tknz in 
  let res = terminal_list expect tknz tok in 
  match res with 
  | Parse xl -> if List.length xl > 0 then res else (reset tknz pos; No_parse)
  | No_parse -> (failwith "terminal_list should never return No_parse")

let lexeme_list expect tknz = 
  let rec _inner expect tknz acc = 
    let pos = mark tknz in 
    let res = expect tknz in 
    match res with 
    | Parse x -> _inner expect tknz (x :: acc) 
    | No_parse -> (reset tknz pos; Parse (List.rev acc))
  in 
  _inner expect tknz []  

let nonempty_lexeme_list expect tknz = 
  let pos = mark tknz in 
  let res = lexeme_list expect tknz in 
  match res with 
  | Parse xl -> if List.length xl > 0 then res else (reset tknz pos; No_parse) 
  | No_parse -> (failwith "lexeme_list should never return No_parse") 

