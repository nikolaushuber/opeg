(* A grammar can have a prolog, a name for the generated toplevel parse function, 
   a list of tokens, the name of the start derivation, 
   and a list of derivation rules *)
type t = {
  header : string option; 
  parser_name: string; 
  start_deriv: string; 
  tokens: tok list; 
  rules: rule list 
}

(* A token has a name, an optional type, and potentially a shortcut *)
and tok = string * string option * string option 

(* Rules have a name, a type, and a list of derivations *)
and rule = string * string * deriv list 

(* A derivation is a list of symbols and a semantic action *)
and deriv = symbol list * action 

(* An action is simply an OCaml code snippet *)
and action = string 

(* Terminal have a name and possibly a lexeme, 
   Nonterminals have a name and a synonym 
*)
and symbol = 
  | Terminal of string * string option 
  | Nonterminal of string * string  

let pp_symbol = function 
| Terminal (name, lex) -> "'" ^ name ^ "'"  ^ 
  (
    match lex with 
    | Some l -> "<" ^ l ^ ">" 
    | None -> "" 
  )
| Nonterminal (name, syn) -> syn ^ " = " ^ name 

let pp_deriv (d : deriv) = 
  let (sym_list, act) = d in 
  "\t/ " ^ String.concat " " (List.map pp_symbol sym_list) ^ " { " ^ act ^ " } " 

let pp_rule (r : rule) = 
  let (name, _type, d_list) = r in 
  name ^ "<" ^ _type ^ ">:\n" ^ 
  String.concat "\n" (List.map pp_deriv d_list) 


let pp (g : t) : string = 
  String.concat "\n" (List.map pp_rule g.rules)
