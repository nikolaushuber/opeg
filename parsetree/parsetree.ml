type symb_suffix = 
  | Optional

type t = {
    header : string option; 
    parser_name: string; 
    start_deriv: string; 
    tokens: pt_tok list; 
    rules: pt_rule list 
  }

and pt_tok = string * string option * string option 

and pt_rule = string * string * pt_alt list

and pt_alt = pt_symb list * string 

and pt_symb = string * string option * symb_suffix option

let to_grammar (pt : t) : Grammar.t = 
  let tok_list = List.map (fun (n, t, s) -> 
    {
      Grammar.Token.name = n;
      ty = t;
      short = s; 
    }) pt.tokens 
  in 

  (* This links shortcuts to actual token names *)
  let tok_short_tbl = 
    let tbl = Hashtbl.create (List.length pt.tokens) in 
    List.iter 
      (
        fun (name, _, short) -> 
          match short with 
          | Some s -> Hashtbl.add tbl s name; Hashtbl.add tbl name name
          | None -> Hashtbl.add tbl name name 
      )
    pt.tokens; 
    tbl 
  in

  let rule_names = List.map ( fun (name, _, _) -> name) pt.rules 
  in 

  let is_a_token (name : string) : bool = 
    Hashtbl.mem tok_short_tbl name 
  in 

  let is_terminal (name : string) : bool = 
    match (List.mem name rule_names, is_a_token name) with
    | (true, false) -> false 
    | (false, true) -> true 
    | (true, true) -> raise (Grammar.Grammar_error (name ^ " is both a terminal and nonterminal symbol"))
    | (false, false) -> raise (Grammar.Grammar_error ("Unknown symbol " ^ name))
  in 

  let rules = List.map (
    fun (name, ty, alt_l) -> 
      let alts = List.map (
        fun (sym_l, act) -> 
          let syms = List.map (
            fun (n, ref, suff) -> 
              let _suff : Grammar.Symbol.suffix option = match suff with 
              | Some Optional -> Some Optional 
              | None -> None 
            in 
              {
                Grammar.Symbol.name = n;
                ref = ref; 
                terminal = is_terminal n; 
                suffix = _suff;
              } 
          ) sym_l in 
          {
            Grammar.Alternative.symbols = syms; 
            action = act; 
          }
      ) alt_l in 
      {
        Grammar.Rule.name = name; 
        ty = ty; 
        alts = alts; 
      }
  ) pt.rules in 

  {
    header = pt.header;
    parser_name = pt.parser_name; 
    start_deriv = pt.start_deriv; 
    tokens = tok_list; 
    token_syns = tok_short_tbl; 
    rules = rules; 
  }
