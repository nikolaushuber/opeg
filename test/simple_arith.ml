let simple_arith_grammar = 
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

let simple_arith = get_grammar simple_arith_grammar

let expect_same g inp = 
  let ptree = Lib.Interpreter.interpret g inp in 
  let res = Lib.Parsetree.to_string ptree in 
  fun () -> Alcotest.(check string) "same string" inp res 

let expect_exn g inp = 
  fun () -> Alcotest.(check_raises) "no parse" Lib.Interpreter.No_parse
  (fun () -> ignore(Lib.Interpreter.interpret g inp)) 

let () = 
  let open Alcotest in 
  run "Simple arithmetics" [
    "Numbers", [
      test_case "Num 1" `Quick (expect_same simple_arith "1"); 
      test_case "Num 2" `Quick (expect_same simple_arith "2") 
    ]; 
    "No parses", [
      test_case "Unknown operator" `Quick (expect_exn simple_arith "1/2"); 
      test_case "Parenthesis missing" `Quick (expect_exn simple_arith "(1+2")
    ]; 
    "Subexpressions", [
      test_case "Simple" `Quick (expect_same simple_arith "(1+2)"); 
      test_case "Simple mul" `Quick (expect_same simple_arith "(1*10)"); 
      test_case "Complex" `Quick (expect_same simple_arith "(1+2)*(3+4)")
    ]
  ]
