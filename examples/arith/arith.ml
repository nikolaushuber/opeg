let rec eval (expr : Parser.ast) : int = match expr with 
  | Num n -> n 
  | Add (e1, e2) -> Int.add (eval e1) (eval e2) 
  | Mul (e1, e2) -> Int.mul (eval e1) (eval e2) 
