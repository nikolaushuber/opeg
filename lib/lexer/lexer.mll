{
    open Parser 

    exception LexingError of string 
}


let whitespace = [' ' '\t' '\r']+ 

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A'-'Z']
let alphanumeric = digit | alpha 
let identifier = (alpha | '_') (alphanumeric | '_')*

let newline = ('\013'* '\010')

rule read_token = parse 
    | "%{" { read_header (Buffer.create 10) lexbuf }

    | ":" { TK_COLON }
    | "/" { TK_CHOICE }
    | "=" { TK_EQUAL }
    | "?" { TK_OPTION }
    | "+" { TK_PLUS }
    | "*" { TK_STAR }
    | "&" { TK_AND }
    | "!" { TK_NOT }
    | "<{" { TK_DEF_START }
    | "}>" { TK_DEF_END }
    | "grammar" { TK_GRAMMAR }
    | "/*" { read_comment lexbuf }
    | "{" { read_semantic_action (Buffer.create 10) 0 lexbuf } 
    | '"' { let id = read_string (Buffer.create 10) lexbuf in TK_STRING(id) } 
    | "r\"" { let id = read_string (Buffer.create 10) lexbuf in TK_REGEX (id) }
    | identifier as id { TK_IDENTIFIER (id) }
    | whitespace { read_token lexbuf }
    | newline { Lexing.new_line lexbuf; read_token lexbuf }
    | eof { TK_EOF }

and read_semantic_action buf depth = parse 
    | "}" { 
        if depth = 0 then 
            TK_SEM_ACTION (Buffer.contents buf)
        else 
            (
                Buffer.add_char buf '}'; 
                read_semantic_action buf (depth-1) lexbuf 
            ) 
        }
    | "{" { Buffer.add_char buf '{'; read_semantic_action buf (depth+1) lexbuf }
    | newline { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_semantic_action buf depth lexbuf }
    | _ as c { Buffer.add_char buf c; read_semantic_action buf depth lexbuf }
    | eof { raise (LexingError "Reached end of file inside semantic action") }

and read_string buf = parse 
    | '"' { (Buffer.contents buf) }
    | '\\' (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as c) { Buffer.add_string buf ((Printf.sprintf "\\%c" c)); read_string buf lexbuf } 
    (* | ['\\' '\'' '\n' '\r' '\t' '\"'] {
        raise (LexingError ("Lexer found unescaped escape character in string: " ^ Lexing.lexeme lexbuf ))
        }
    | [^ '"' '\\' '\'' '\n' '\r' '\t' '\"']+ as lxm { Buffer.add_string buf lxm; read_string buf lexbuf } *)
    | _ as c { Buffer.add_char buf c; read_string buf lexbuf }
    | eof { raise (LexingError "EOF found while reading string") }
    | _ { raise (LexingError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

and read_header buf = parse 
    | "%}" { TK_HEADER (Buffer.contents buf) }
    | newline { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_header buf lexbuf }
    | _ as c { Buffer.add_char buf c; read_header buf lexbuf }
    | eof { raise (LexingError "Reached end of file inside header definition") }

and read_comment = parse 
    | "*/" { read_token lexbuf }
    | newline { Lexing.new_line lexbuf; read_comment lexbuf } 
    | eof { raise (LexingError "Feached end of file inside comment") }
    | _ { read_comment lexbuf }
