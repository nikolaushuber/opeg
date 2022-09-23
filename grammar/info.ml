open Main 

module StringSet = Set.Make(String)

type left_rec = 
  | No 
  | Direct 
  | Hidden 
  | Indirect 

type maybe_empty = bool 

type rule_info = 
{
  left : left_rec;
  empty : maybe_empty; 
}

(* A call graph is an associative list of rule names, i.e. 
   if a grammar looks like this: 
    expr -> expr '+' term | term 
    term -> term '*' atom | atom 
    atom -> NUM | '(' expr ')'
  with three non-terminal symbols expr, term, and atom then the list would look 
  like this:
  [
    ("expr", {"expr", "term", "atom"});
    ("term", {"term", "atom"}); 
    ("atom", {})
  ]
*)
type call_graph = (string * StringSet.t) list 

let ( === ) cg1 cg2 = List.for_all (
  fun (name, new_set) -> StringSet.equal new_set (List.assoc name cg2) 
) cg1

let get_firsts (g : t) : call_graph = 
  (* For a given alternative, give the list of names of non-terminal symbols 
     that could come in first position 
  *)
  let firsts_of_alt (a : Alternative.t) : StringSet.t = 
    let rec get_first_symbols symbs acc = match symbs with 
    | s :: rest -> (
      match s.Symbol.terminal with 
      | true -> (
        (* s is a terminal symbol *)
        match s.Symbol.suffix with 
        | Empty | Plus -> acc 
        | Optional | Star -> get_first_symbols rest acc 
      )
      | false -> 
        (* s is not a terminal symbol *)
        match s.Symbol.suffix with 
        | Empty | Plus -> StringSet.add s.Symbol.name acc 
        | Optional | Star -> get_first_symbols rest (StringSet.add s.Symbol.name acc) 
    )
    | [] -> acc
    in 
    
    let all_symbs = a.symbols in 
    get_first_symbols all_symbs StringSet.empty 
  in 

  let firsts_of_rule (r : Rule.t) : StringSet.t = 
    List.fold_left 
      (fun init alt -> StringSet.union init (firsts_of_alt alt)) 
      StringSet.empty r.Rule.alts
  in 

  let rules = g.rules in 
  let firsts = List.map (fun r -> (r.Rule.name, firsts_of_rule r)) rules in 

  let rec trans_closure (cg : call_graph) : call_graph = 
    let new_cg = List.map (fun (name, set) -> 
      let new_set = StringSet.fold (fun s init -> 
        StringSet.union (List.assoc s cg) init) set set in 
        (name, new_set) 
      ) cg in 
    if cg === new_cg then new_cg else trans_closure new_cg
  in 

  trans_closure firsts

let pp_call_graph (cg : call_graph) : string = 
  "[\n\t" ^ 
    String.concat "\n\t" (
      List.map (fun (name, set) -> 
        name ^ " -> " ^ 
        String.concat " | " (StringSet.fold (fun s init -> s :: init ) set [])
      ) cg 
    ) ^ 
  "\n]"
