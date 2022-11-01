module G = Grammar
module P = Parsetree

exception No_parse 

let interpret (g : G.t) (inp : string) : P.t = 
  match g.rules with 
  (* Empty grammars always fail *)
  | [] -> failwith "No parse" 
  | _ -> begin
    let (_, start) = List.hd g.rules in 
    let state = G.State.make inp g in 
    match G.Rule.eval state start with 
    | (_, No_parse) -> raise No_parse
    | (next, Parse tree) -> 
      if Int.equal next.pos (String.length inp) then tree
      else raise No_parse
  end

