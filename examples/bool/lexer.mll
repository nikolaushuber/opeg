{
    open Parser 
}

let whitespace = [' ' '\t' '\r']+ 

rule read_token = parse 
    | "(" { LPAREN }
    | ")" { RPAREN }
    | whitespace {  }
