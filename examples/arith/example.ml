open OpegLib
(* Begin header *)

  type ast = 
      | Add of ast * ast 
      | Mul of ast * ast 
      | Num of int 
  
(* End header *)

let rec start state =
 let alt_0 state =
  let* e = expr state in
  let* _ = expect_eof state in
  Parse (  e  )
 in 
 try_in_seq [alt_0] state

and expr state =
 let alt_0 state =
  let* t = term state in
  let* _ = parse_string state "+" in
  let* e = expr state in
  Parse (  Add(t, e)  )
 in 

 let alt_1 state =
  let* t = term state in
  Parse (  t  )
 in
 try_in_seq [alt_0; alt_1] state

and term state =
 let alt_0 state =
  let* a = atom state in
  let* _ = parse_string state "*" in
  let* t = term state in
  Parse (  Mul(a, t)  )
 in
 let alt_1 state =
  let* a = atom state in
  Parse (  a  )
 in
 try_in_seq [alt_0; alt_1] state

and atom state =
 let alt_0 state =
  let* n = number state in
  Parse (  Num( n )  )
 in
 let alt_1 state =
  let* _ = parse_string state "(" in
  let* e = expr state in
  let* _ = parse_string state ")" in
  Parse (  e  )
 in
 try_in_seq [alt_0; alt_1] state

and number state =
 let alt_0 state =
  let* n = parse_regex state "[1-9][0-9]*" in
  Parse (  int_of_string n  )
 in
 try_in_seq [alt_0] state
