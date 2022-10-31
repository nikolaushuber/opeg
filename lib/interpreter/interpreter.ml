module G = Grammar
module P = Parsetree

exception No_parse 

let interpret (g : G.t) (inp : string) : P.t = 
  match g.rules with 
  (* Empty grammars always fail *)
  | [] -> failwith "No parse" 
  | _ -> begin
    let (_, start) = List.hd g.rules in 
    match G.Rule.eval inp g start 0 with 
    | No_parse -> raise No_parse
    | Parse (next, tree) -> 
      if Int.equal next (String.length inp) then tree
      else raise No_parse
  end

