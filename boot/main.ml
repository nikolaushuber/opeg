let main file_name out_name = 
  let oc = open_out (out_name ^ ".ml") in 
  let ic = open_in file_name in 
  let lexbuf = Lexing.from_channel ic in 
  let pt = Boot_parser.parse Lexer.read_token lexbuf in  
  let grammar = Parsetree.to_grammar pt in 
  
  Printf.fprintf oc "%s" (Utils.string_of_grammar grammar); 

  close_out oc;  
  close_in ic

let usage_msg = "opeg <file> -o <output>" 
let input_file = ref "" 
let output_file = ref "" 

let anon_fun filename = 
  input_file := filename 

let speclist = 
  [("-o", Arg.Set_string output_file, "Set output file name")] 

let () = 
  Arg.parse speclist anon_fun usage_msg; 
  let out_file = 
    if String.equal "" !output_file then 
      Filename.remove_extension !input_file
    else 
      Filename.remove_extension !output_file 
  in 
  main !input_file out_file  
