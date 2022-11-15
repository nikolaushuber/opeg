open Lib 

let usage_msg = "opeg <file> -o <output>" 
let input_file = ref "" 
let output_file = ref "" 

let anon_fun filename = 
  input_file := filename 

let speclist = 
  [
    ("-o", Arg.Set_string output_file, "Set output file name"); 
  ] 


let () = 
  Arg.parse speclist anon_fun usage_msg; 
  (* Did we specify an input file? *)
  match String.equal "" !input_file with 
  | false -> begin  
    let basename = Filename.remove_extension (Filename.basename !input_file) in 
    let out_file = 
      if String.equal "" !output_file then 
        basename ^ ".ml" 
    else 
        !output_file
    in
    let g = Grammar_utils.file_to_grammar !input_file in 
    let oc = open_out out_file in 
    let fmt = Format.formatter_of_out_channel oc in 
    let fmt_funcs = Format.pp_get_formatter_out_functions fmt () in 
    let fmt_funcs = {fmt_funcs with 
      out_indent = (fun n -> fmt_funcs.out_string (String.make (2*n) ' ') 0 (2*n))} 
    in 
    Format.pp_set_formatter_out_functions fmt fmt_funcs; 
    Gen.gen_grammar fmt g; 
    close_out oc 
  end
  | true -> failwith "No input file provided" 
