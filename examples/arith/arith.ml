let rec eval (expr : Parser.ast) : int = match expr with 
  | Num n -> n 
  | Add (e1, e2) -> Int.add (eval e1) (eval e2)
  | Sub (e1, e2) -> Int.sub (eval e1) (eval e2) 
  | Mul (e1, e2) -> Int.mul (eval e1) (eval e2) 
  | Div (e1, e2) -> Int.div (eval e1) (eval e2) 

let interpret (s : string) : int = 
  Lexing.from_string s 
  |> Parser.parse Lexer.read_token 
  |> eval 

