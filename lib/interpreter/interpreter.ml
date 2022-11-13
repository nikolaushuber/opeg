module G = Grammar
module P = Parsetree

exception No_parse 

let interpret (g : G.t) (inp : string) : P.t = 
  match g.rules with 
  (* Empty grammars always fail *)
  | [] -> failwith "Cannot parse with empty grammar" 
  | _ -> begin
    let (_, start_rule) = List.hd g.rules in 
    let state = G.State.make inp g in 
    match G.Rule.eval state start_rule with 
    | (_, No_parse) -> raise No_parse
    | (next, Parse tree) -> 
      if Int.equal next.pos (String.length inp) then 
        (* match tree.node with 
        | Tree list -> begin
          let first = List.hd list in 
          match first.node with 
          | Tree _ -> first 
          | _ -> failwith "Expected a tree here"
        end
        | _ -> failwith "Expected a tree here too"  *)
        tree
      else failwith "Input left"
  end

