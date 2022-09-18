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
    | "parser" { TOK_KW_PARSER }
    | "token" { TOK_KW_TOKEN }
    | "%{" (_* as hd) "%}" { TOK_HEADER (hd) }

    | ":" { TOK_DOUBLE_DOTS }
    | "/" { TOK_CHOICE }
    | "=" { TOK_EQUALS }
    | "?" { TOK_OPTION }
    | "+" { TOK_PLUS }
    | "*" { TOK_STAR }
    | "<" { read_type (Buffer.create 10) lexbuf }
    | "%%" { TOK_SEC_DIVIDE }
    | "|" { TOK_BAR }
    | "{" { read_semantic_action (Buffer.create 10) 0 lexbuf } 
    | "\"" (['!'-'~']* as id) "\"" { TOK_NAME (id) }
    | identifier as id { TOK_NAME (id) }
    | whitespace { read_token lexbuf }
    | newline { Lexing.new_line lexbuf; read_token lexbuf }
    | eof { TOK_EOF }

and read_semantic_action buf depth = parse 
    | "}" { 
        if depth = 0 then 
            TOK_SEM_ACTION (Buffer.contents buf)
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

and read_type buf = parse 
    | ">" { TOK_TYPE (Buffer.contents buf) }
    | newline { Lexing.new_line lexbuf; read_type buf lexbuf }
    | _ as c {Buffer.add_char buf c; read_type buf lexbuf }
    | eof { raise (LexingError "Reached end of file inside type information") }
