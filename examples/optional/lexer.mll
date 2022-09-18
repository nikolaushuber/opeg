{
    open Parser 
}

rule read_token = parse 
    | "a" { TOK_A }
    | "b" { TOK_B }
    | "c" { TOK_C }
    | eof { TOK_EOF }
