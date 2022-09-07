{
  open Parser
}

let whitespace = [' ' '\t' '\r']+ 
let digit = ['0' - '9']
let integer = '0' | ['1'-'9'] (digit)* 

rule read_token = parse 
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "+" { ADD }
    | "-" { SUB }
    | "*" { MUL }
    | "/" { DIV }
    | whitespace { read_token lexbuf }
    | integer as i { NUMBER(int_of_string i) }
    | eof { EOF }

