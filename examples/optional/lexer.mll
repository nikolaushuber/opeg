{
    open Parser 
}

let whitespace = [' ' '\t' '\r']+ 
let digit = ['0' - '9']
let integer = '0' | ['1'-'9'] (digit)*

rule read_token = parse 
    | "a" { TOK_A }
    | "b" { TOK_B }
    | "c" { TOK_C }
    | whitespace { read_token lexbuf }
    | integer as i { TOK_NUM(int_of_string i) }
    | eof { TOK_EOF }
