let expand_grammars (glist : (string * Grammar.t) list) : (string * Grammar.t) list = 
  let exp_list = List.fold_left (fun list grammar -> 
    let (name, g) = grammar in 
    if List.length g.Grammar.parts == 0 then 
      (name, g) :: list 
    else
      let grammar_parts = List.map (fun part -> List.assoc part list) g.Grammar.parts in 
      (name, List.fold_left (Grammar.(++)) Grammar.empty grammar_parts) :: list
  ) [] glist in 
  List.rev exp_list

let file_to_grammar_list path : (string * Grammar.t) list = 
  let ic = open_in path in 
  let lbuf = Lexing.from_channel ic in 
  expand_grammars (Parser.parse Lexer.read_token lbuf)

let string_to_grammar_list str : (string * Grammar.t) list = 
  let lbuf = Lexing.from_string str in 
  expand_grammars (Parser.parse Lexer.read_token lbuf)

let file_to_grammar path : Grammar.t = 
  let grammars = file_to_grammar_list path in 
  let (_, g) = List.hd (List.rev grammars) in 
  g 

let string_to_grammar str : Grammar.t = 
  let grammars = string_to_grammar_list str in 
  let (_, g) = List.hd (List.rev grammars) in 
  g 
