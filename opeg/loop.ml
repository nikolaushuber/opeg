let help_msg = String.concat "\n" [
    "Available commands:"; 
    "  load <file>   -> loads a grammar";
    "  parse \"...\"   -> parses the given string according to the loaded grammar"; 
    "  close         -> closes the interpreter"; 
    "  json          -> prints the json serialization of the loaded grammar"; 
    "  help          -> prints this help message"; 
  ]

let command_regex = Re.compile (Re.rep1 Re.space)

let out msg = 
  print_endline (">>> " ^ 
    String.concat "\n>>> " (String.split_on_char '\n' msg))

let out_err msg = 
  print_endline ("!!! " ^ msg)

let loaded_grammar : Grammar.t option ref = ref None 

let load (args : string list) : unit = 
  if List.length args <> 1 then 
    out_err "load expects one argument, a path to a file"
  else
    try 
      let ic = open_in (List.nth args 0) in 
      let lbuf = Lexing.from_channel ic in 
      loaded_grammar := Some (Parser.start Lexer.read_token lbuf) 
    with 
      | Sys_error _ -> out_err ("Cannot find file " ^ (List.nth args 0))

let parse (args : string list) : unit = 
  if List.length args <> 1 then 
    out_err "parse expects one argument, the string to parse"
  else
    if Option.is_none !loaded_grammar then 
      out_err "No grammar loaded" 
    else
      let str = List.nth args 0 in 
      let grammar = Option.get !loaded_grammar in 
      let state = Interpreter.init_interp grammar str in 
      let ret = Interpreter.parse state in  
      if ret then 
        out "Parsed successfully" 
      else 
        out "Parse failure"

let close (_ : string list) : unit = exit 0 
let json (_ : string list) : unit = 
  if Option.is_none !loaded_grammar then 
    out_err "No grammar loaded" 
  else
    let grammar = Option.get !loaded_grammar in 
    Grammar.Json.string_of_t grammar 
    |> Yojson.Safe.prettify 
    |> out  

let help (_ : string list) : unit = 
  out help_msg 

let cmds : (string * (string list -> unit)) list = [
  ("load", load);
  ("parse", parse); 
  ("close", close); 
  ("json", json); 
  ("help", help); 
]

let eval_input (cmd : string) = 
  if String.length cmd == 0 then () 
  else 
  try 
    let str_list = Re.split command_regex cmd in 
    let cmd_str = List.hd str_list in 
    let func = List.assoc cmd_str cmds in 
    func (List.tl str_list)
  with
    | Not_found -> out_err ("Input error - Unknown command " ^ cmd)
    | Failure _ -> out_err ("Input error") 

let loop (g : Grammar.t option) = 
  loaded_grammar := g; 

  let rec _inner () = 
    print_string "*> "; 
    let inp = read_line () in 
    eval_input inp; 
    print_endline ""; 
    _inner () 
  in 

  _inner () 

