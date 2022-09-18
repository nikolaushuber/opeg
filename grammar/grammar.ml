exception Grammar_error of string

module Symbol = struct 
  type suffix = 
    | Empty 
    | Optional 
    | Plus 
    | Star 

  type t = 
  {
    name : string; 
    ref : string option; 
    terminal : bool;
    suffix : suffix; 
  }

  let pp (s : t) : string = 
    (if Option.is_some s.ref then (Option.get s.ref) ^ " = " else "")
    ^ (if s.terminal then "\"" ^ s.name ^ "\"" else s.name) ^ 
    match s.suffix with 
    | Optional -> "?" 
    | Empty -> "" 
    | Plus -> "+" 
    | Star -> "*" 
end 

module Action = struct 
  type t = string 

  let pp (a : t) : string = "{ " ^ a ^ " }"
end 

module Alternative = struct 
  type t = 
  {
    symbols : Symbol.t list; 
    action : Action.t; 
  }

  let pp (a : t) : string = 
    "/ " ^ String.concat " " (List.map Symbol.pp a.symbols) ^ " " ^ 
    Action.pp a.action
end 

module Rule = struct 
  type t = 
  {
    name : string;
    ty : string; 
    alts : Alternative.t list; 
  }

  let pp (r : t) : string = 
    r.name ^ " <" ^ r.ty ^ ">:\n\t" ^ 
    String.concat "\n\t" (List.map Alternative.pp r.alts)  
end 

module Token = struct 
  type t = 
  {
    name : string; 
    ty : string option; 
    short : string option; 
  }

  let pp (tok : t) : string = 
    "[" ^ tok.name ^ 
    (
      match tok.ty with 
      | Some s -> "," ^ s
      | None -> "" 
    ) ^  
    (
      match tok.short with 
        | Some s -> ",\"" ^ s ^ "\""
        | None -> "" 
    )
    ^ "]"
end 

(* A grammar can have a header, a name for the generated toplevel parse function, 
   a list of tokens, the name of the start derivation, 
   and a list of derivation rules *)
type t = 
{
  header : string option; 
  parser_name: string; 
  start_deriv: string; 
  tokens: Token.t list; 
  token_syns: (string, string) Hashtbl.t; 
  rules: Rule.t list;  
}


let pp (g : t) : string = 
  "Parser " ^ g.parser_name ^ "\n" ^ 
  "Tokens: " ^ String.concat ";" (List.map Token.pp g.tokens) ^ "\n" ^ 
  String.concat "\n" (List.map Rule.pp g.rules) ^ "\n"
