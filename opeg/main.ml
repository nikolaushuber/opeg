let main file_name out_name = 
  let oc = open_out (out_name ^ ".ml") in 
  let ic = open_in file_name in 
  let lexbuf = Lexing.from_channel ic in 
  let grammar = Parser.parse Lexer.read_token lexbuf in  

  Printf.fprintf oc "%s\n\n" (Utils.gen_header grammar); 
  Printf.fprintf oc "%s\n\n" (Utils.gen_token_type grammar); 
  Printf.fprintf oc "%s\n\n" (Utils.tokenizer_string); 
  Printf.fprintf oc "%s\n\n" (Utils.gen_expect_functions grammar); 
  Printf.fprintf oc "%s\n\n" (Utils.gen_hash_tables grammar); 
  Printf.fprintf oc "%s\n\n" (Utils.lookup_or_compute_string); 
  Printf.fprintf oc "%s\n\n" (Utils.gen_derivations grammar); 
  Printf.fprintf oc "%s\n\n" (Utils.gen_toplevel_fun grammar); 

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
