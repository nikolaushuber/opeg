open Grammar

let e : pattern = { ty = Reference ("expr", NoMod); ref = Some "e" }
let t : pattern = { ty = Reference ("term", NoMod); ref = Some "t" }
let plus : pattern = { ty = Quote "+"; ref = None }

let expr_alt_1 : choice = { name = "add"; patterns = [t; plus; e] }
let expr_alt_2 : choice = { name = "t"; patterns = [t] }

let expr = { name = "expr"; choices = [expr_alt_1; expr_alt_2]; pos = (0, 0, 0, 0) }

let a : pattern = { ty = Reference ("atom", NoMod); ref = Some "a" }
let mul : pattern = { ty = Quote "*"; ref = None }

let term_alt_1 : choice = { name = "mul"; patterns = [a; mul; t] }
let term_alt_2 : choice = { name = "a"; patterns = [a] }

let term = { name = "term"; choices = [term_alt_1; term_alt_2]; pos = (0, 0, 0, 0) }

let num_pattern : pattern = { ty = Regex "[1-9][0-9]*"; ref = None }
let num_choice : choice = { name = "n"; patterns = [num_pattern] }
let num  = { name = "NUM"; choices = [num_choice]; pos = (0, 0, 0, 0) }

let n : pattern = { ty = Reference ("NUM", NoMod); ref = Some "n" }
let l_paren : pattern = { ty = Quote "("; ref = None }
let r_paren : pattern = { ty = Quote ")"; ref = None }

let atom_alt_1 : choice = { name = "num"; patterns = [n] }
let atom_alt_2 : choice = { name = "subex"; patterns = [l_paren; e; r_paren ] }

let atom = { name = "atom"; choices = [atom_alt_1; atom_alt_2]; pos = (0, 0, 0, 0) }
let grammar : t = { name = "Simple arithmetic"; lexing = [num]; rules = [expr; term; atom]; whitespace = [" "; "\n"; "\r"; "\t" ]; start = "expr" }

(* let () = 
  Grammar.Json.string_of_t grammar 
  |> Yojson.Safe.prettify 
  |> print_endline  *)

(* let () = 
  let state = Interpreter.init_interp grammar "1 + (2*3) * 4" in 
  if Interpreter.parse state then 
    print_endline "Success" 
  else 
    print_endline "Failure" *)
