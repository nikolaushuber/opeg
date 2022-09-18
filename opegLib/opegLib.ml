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
