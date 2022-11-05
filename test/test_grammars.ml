let simple_arith_grammar_str = 
  {|grammar arith = <{%{
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
    }>
  |}


let simple_arith_grammar = Lib.Grammar_utils.string_to_grammar simple_arith_grammar_str
