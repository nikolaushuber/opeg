open Lib 

let version = "0.1" 

let welcome = String.concat "\n" [
    "Welcome to Opeg!"; 
    "Version " ^ version ^ " running on " ^ Sys.os_type ; 
    "For more information type \"help\""; 
  ]

let usage_msg = "opeg <file> -o <output>" 

let print_ir = ref false 
let input_file = ref "" 
let output_file = ref "" 

let anon_fun filename = 
  input_file := filename 

let speclist = 
  [
    ("-o", Arg.Set_string output_file, "Set output file name"); 
    ("-ir", Arg.Set print_ir, "Print the grammar in JSON format"); 
  ] 


let () = 
  Arg.parse speclist anon_fun usage_msg; 
  (* Did we specify an input file? *)
  match String.equal "" !input_file with 
  (* Yes, so perform the given work *)
  | false -> begin 
    let ic = open_in !input_file in 
    let lbuf = Lexing.from_channel ic in 
    let g = Parser.start Lexer.read_token lbuf in 
    if !print_ir then 
      Grammar.Json.string_of_t g 
      |> Yojson.Safe.prettify 
      |> print_endline  
    ; 
    if String.equal "" !output_file then 
      Loop.loop (Some g) 
  end 
  (* No, therfore start the interactive loop *)
  | true -> begin 
    print_endline welcome; 
    print_endline ""; 
    Loop.loop None  
  end 
