let simple_arith_grammar_str = 
  {|%{
    type ast = 
        | Add of ast * ast 
        | Mul of ast * ast 
        | Num of int 
    %}
    
    expr: 
          t = term "+" e = expr { Add(t, e) }
        / t = term { t }
    
    term: 
          a = atom "*" t = term { Mul(a, t) }
        / a = atom { a }
    
    atom: 
          n = number { Num( n ) }
        / "(" e = expr ")" { e }
      
    number: n = r"[1-9][0-9]*" { int_of_string n }
  |}

let get_grammar str = 
  let lbuf = Lexing.from_string str in 
  Lib.Parser.parse Lib.Lexer.read_token lbuf

let simple_arith_grammar = get_grammar simple_arith_grammar_str
