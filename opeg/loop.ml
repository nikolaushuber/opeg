open Lib

let help_msg = String.concat "\n" [
    "Available commands:"; 
    "  load <file>   -> loads a grammar";
    "  parse \"...\"   -> parses the given string according to the loaded grammar"; 
    "  close         -> closes the interpreter"; 
    "  json          -> prints the json serialization of the loaded grammar"; 
    "  help          -> prints this help message"; 
  ]

let command_regex = Re.compile (Re.rep1 Re.space)
type cmd_descr = string * (string * string * (string -> unit))

let out msg = 
  print_endline (String.concat "\n" (String.split_on_char '\n' msg) ^ "\n")

let out_err msg = 
  print_endline ("!!! " ^ msg ^ "\n")

let loaded_grammar : Grammar.t option ref = ref None 

let extract_filename_regex = Re.Perl.compile_pat "[\"]?([^\"]*)[\"]?" 
let load (args : string) : unit =
  try  
    let path = Re.Group.get (Re.exec extract_filename_regex args) 1 in 
    begin
      try 
        let ic = open_in path in 
        let lbuf = Lexing.from_channel ic in 
        loaded_grammar := Some (Parser.start Lexer.read_token lbuf) 
      with 
        | Sys_error _ -> out_err ("Cannot find file " ^ path)
    end 
  with 
    
    | Not_found -> out_err "load expects one argument, a path to a file"

let extract_string_regex = Re.Perl.compile_pat "\"([^\"]*)\"" 
let parse (args : string) : unit = 
  try 
    let str = Re.Group.get (Re.exec extract_string_regex args) 1 in 
    if Option.is_none !loaded_grammar then 
      out_err "No grammar loaded" 
    else
      let grammar = Option.get !loaded_grammar in 
      let state = Interpreter.init_interp grammar str in 
      try 
        let res = Interpreter.parse state in  
        Parsetree.pp_ptree Format.std_formatter res; 
        print_endline "" 
      with 
        | Failure _ -> out_err "Parse failure"
  with 
    Not_found -> out_err "Argument error: parse expects a string as argument"

let close (_ : string) : unit = exit 0 

let json (_ : string) : unit = 
  if Option.is_none !loaded_grammar then 
    out_err "No grammar loaded" 
  else
    let grammar = Option.get !loaded_grammar in 
    Grammar.Json.string_of_t grammar 
    |> Yojson.Safe.prettify 
    |> out  

let clear (_: string) : unit = 
  let _ = Sys.command "clear" in 
  ()

let rec cmds : cmd_descr list = [
  ("load", ("load <file>", "loads a grammar from <file>", load));
  ("parse", ("parse \"...\"", "parses the given string", parse)); 
  ("close", ("close", "closes the interpreter", close)); 
  ("json", ("json", "prints out json representation of loaded grammar", json)); 
  ("help", ("help", "prints this help message", help)); 
  ("clear", ("clear", "clears the screen", clear));
]

and help (_ : string) : unit = 
  let str = "Available commands: \n" ^  
  String.concat "\n" (List.map (
    fun (_, (name, descr, _)) -> "  " ^ name ^ " -> " ^ descr
  ) cmds) in 
  out str 

let extract_cmd_regex = Re.Perl.compile_pat "(\\w*)\\b[ \n\t\r]*(.*)"
let eval_input (cmd : string) = 
  try 
    let matched = Re.exec extract_cmd_regex cmd in 
    let cmd_str = Re.Group.get matched 1 in 
    let (_, _, func) = List.assoc cmd_str cmds in 
    func (Re.Group.get matched 2)
  with
    | Not_found -> out_err ("Input error - Unknown command " ^ cmd)
    | Failure _ -> out_err ("Input error") 

let loop (g : Grammar.t option) = 
  loaded_grammar := g; 

  let rec _inner () = 
    print_string "opeg> "; 
    let inp = read_line () in 
    eval_input inp; 
    _inner () 
  in 

  _inner () 

