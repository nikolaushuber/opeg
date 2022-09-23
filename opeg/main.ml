let usage_msg = "opeg <file> -o <output>" 
let print_grammar = ref false 
let print_firsts = ref false 
let input_file = ref "" 
let output_file = ref "" 

let get_grammar file_name : Grammar.t = 
  let ic = open_in file_name in 
  let lexbuf = Lexing.from_channel ic in 
  let pt = Parser.parse Lexer.read_token lexbuf in 
  let grammar = Parsetree.to_grammar pt in  
  close_in ic; 
  grammar 

let main grammar out_name = 
  let oc = open_out (out_name ^ ".ml") in 
  Printf.fprintf oc "%s\n\n" (Utils.string_of_grammar grammar); 
  close_out oc 

let anon_fun filename = 
  input_file := filename 

let speclist = 
  [
    ("-o", Arg.Set_string output_file, "Set output file name"); 
    ("--print-grammar", Arg.Set print_grammar, "Pretty print the grammar"); 
    ("--print-firsts", Arg.Set print_firsts, "Pretty print the list of first symbols")
  ] 

let () = 
  Arg.parse speclist anon_fun usage_msg; 
  let grammar = get_grammar !input_file in 
  if !print_grammar then print_endline (Grammar.pp grammar); 
  if !print_firsts then 
    let cg = Grammar.Info.get_firsts grammar in  
    print_endline (Grammar.Info.pp_call_graph cg);
  ; (* <-- otherwise computation does not continue *)
  if Bool.not (String.equal "" !output_file) then 
    main grammar (Filename.remove_extension !output_file)
