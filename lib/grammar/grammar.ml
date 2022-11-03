module P = Parsetree 

let print_stack (stack : (int * string * int) list) : unit = 
  let list = List.map (fun (pos,s,c) -> "(" ^ string_of_int pos ^ "," ^ s ^ "," ^ string_of_int c ^ ")") stack in 
  let str = String.concat " -> " (List.rev list) in 
  print_endline str

module rec Parse_result : sig 
  type t =
    | Parse of P.t
    | No_parse

  val return : ?name:string option -> State.t -> P.pos -> P.node -> t
end = struct
  type t =
    | Parse of P.t
    | No_parse

  let return ?(name = None) (state : State.t) pos node : t = 
    let (_, rule, choice) = List.hd state.stack in 
    Parse {
      name = name; 
      pos = pos; 
      node = node; 
      rule = rule; 
      choice = choice; 
    }
end

and Parse_expr : sig 
  type t = 
    | Match of Match_expr.t 
    | Predicate of Predicate_expr.t 
    | Repetition of Repetition_expr.t 
    | Reference of string 

  val eval : State.t -> t -> State.t * Parse_result.t
end = struct 
  type t = 
    | Match of Match_expr.t 
    | Predicate of Predicate_expr.t 
    | Repetition of Repetition_expr.t 
    | Reference of string 

  let eval (state : State.t) p : State.t * Parse_result.t = match p with 
    | Match m -> Match_expr.eval state m
    | Predicate pred -> Predicate_expr.eval state pred
    | Repetition rep -> Repetition_expr.eval state rep
    (* TODO: Use hashtbl instead *)
    | Reference r -> begin match List.assoc_opt r state.grammar.rules with 
      | Some rule -> 
        let new_state = {state with stack = (state.pos, r, 0) :: state.stack} in 
         Rule.eval new_state rule 
      | None -> failwith "Holes in grammar not yet supported" 
  end
end

and Repetition_expr : sig 
  type t = 
    | Zero_or_one of Parse_expr.t  
    | Zero_or_more of Parse_expr.t 
    | One_or_more of Parse_expr.t 

  val eval : State.t -> t -> State.t * Parse_result.t
end = struct 
  type t = 
    | Zero_or_one of Parse_expr.t
    | Zero_or_more of Parse_expr.t
    | One_or_more of Parse_expr.t

  let receval state p = 
    let rec inner state' p' acc : State.t * Parsetree.t list = 
      match Parse_expr.eval state' p' with 
      | (next, Parse tree) -> inner next p' (tree :: acc)
      | (next, No_parse) -> (next, List.rev acc)
    in 
    inner state p []

  let eval state rep : State.t * Parse_result.t = match rep with 
    | Zero_or_one p -> begin 
      match Parse_expr.eval state p with 
      | (next, Parse tree) -> (next, Parse_result.return next tree.pos (Option (Some tree)))
      | (next, No_parse) -> (next, Parse_result.return next (next.pos, next.pos) (Option None))
    end
    | Zero_or_more p -> 
      begin match receval state p with
        | (next, []) -> (next, Parse_result.return next (next.pos, next.pos) (Tree []))
        | (next, res) -> (next, Parse_result.return next (state.pos, next.pos) (Tree res))
      end 
    | One_or_more p -> 
      begin match receval state p with 
        | (next, []) -> (next, No_parse)
        | (next, res) -> (next, Parse_result.return next (state.pos, next.pos) (Tree res))
      end
end 

and Predicate_expr : sig
  type t = 
    | And of Parse_expr.t
    | Not of Parse_expr.t

  val eval : State.t -> t -> State.t * Parse_result.t
end = struct 
  type t = 
    | And of Parse_expr.t
    | Not of Parse_expr.t

  let eval state p : State.t * Parse_result.t = match p with 
    | And p' -> 
      begin match Parse_expr.eval state p' with 
      | (_, Parse tree) -> (state, Parse tree)
      | (next, No_parse) -> (next, No_parse) 
      end 
    | Not p' ->  
      begin match Parse_expr.eval state p' with 
      | (_, Parse _) -> (state, No_parse)
      | (_, No_parse) -> (state, Parse_result.return state (state.pos, state.pos) (Option None))
      end 
end

and Match_expr : sig 
  type t = 
    | Quote of string 
    | Regex of string 
  
  val eval : State.t -> t -> State.t * Parse_result.t 
end = struct
 type t = 
    | Quote of string 
    | Regex of string 
  
  let eval (state : State.t) m : State.t * Parse_result.t = match m with 
    | Quote q -> begin 
      try 
        let sub = String.sub state.input state.pos (String.length q) in 
        if (String.starts_with ~prefix:q sub) then 
          let len = String.length q in 
          let next_pos = state.pos + len in 
          let next = {state with pos = next_pos} in 
          (next, Parse_result.return next (state.pos, next_pos) (Lexeme q))
        else 
          (state, No_parse)
      with
        (* Maybe we can rewrite this with a pattern match? *)
        | _ -> (state, No_parse) 
    end
    | Regex r -> begin 
      let rx = Re.Perl.compile_pat r in 
      try 
        let ret = Re.exec ~pos:state.pos rx state.input in 
        let (start, len) = Re.Group.offset ret 0 in 
        if Int.equal start state.pos then 
          let next = {state with pos = len} in 
          let res = Re.Group.get ret 0 in 
          (next, Parse_result.return next (state.pos, len) (Lexeme res))
        else 
          (state, No_parse)
      with
        | Not_found -> (state, No_parse)
    end 
end

and Choice : sig 
  type t = {
    symbols : (string option * Parse_expr.t) list; 
    action : string; 
  }

  val eval : State.t -> t -> State.t * Parse_result.t 
end = struct 
  type t = {
    symbols : (string option * Parse_expr.t) list; 
    action : string; 
  }

  let eval (state : State.t) c = 
    let rec inner syms state' acc : State.t * Parse_result.t = match syms with 
      | [] -> (state', Parse_result.return state (state.pos, state'.pos) (Tree (List.rev acc)))
      | (name, p) :: xs -> 
        begin match Parse_expr.eval state' p with 
        | (next, No_parse) -> (next, No_parse)    (* => next state is irrelevant *)
        | (next, Parse tree) -> inner xs {next with stack = state'.stack} ({tree with name = name} :: acc)
        end
    in 
    inner c.symbols state []
end

and Rule : sig 
  type t = Choice.t list

  val eval : State.t -> t -> State.t * Parse_result.t 
end = struct 
  type t = Choice.t list

  let rec eval state c : State.t * Parse_result.t = match c with 
    | [] -> (state, No_parse) 
    | c :: cs -> begin match Choice.eval state c with 
      | (_, Parse _) as p -> p 
      | (_, No_parse) -> 
        let (pos, rule, choice) = List.hd state.stack in 
        eval {state with stack = (pos, rule, choice+1) :: List.tl state.stack} cs 
    end
end 

(* This is just internal to allow recusive module definition *)
and Gramm_ : sig 
  type t = {
    rules : (string * Rule.t) list; 
    header : string option; 
  }
end = struct 
  type t = {
    rules : (string * Rule.t) list; 
    header : string option; 
  }
end 

and State : sig 
  type t = {
    input : string; 
    pos : int; 
    grammar : Gramm_.t; 
    memo : ((int * string), (int * P.t)) Hashtbl.t; 
    stack : (int * string * int) list; 
  }

  val delta : t -> t -> (int * int)
  val make : string -> Gramm_.t -> t 
end = struct 
  type t = {
    input : string; 
    pos : int; 
    grammar : Gramm_.t; 
    memo : ((int * string), (int * P.t)) Hashtbl.t;
    stack : (int * string * int) list; 
  }

  let delta state1 state2 = (state1.pos, state2.pos)

  let make inp g = {
    input = inp;
    pos = 0;
    grammar = g;
    memo = Hashtbl.create 10;
    stack = [0, fst (List.hd g.rules), 0];
  }
(* 
  and f_translate_pos (str : string) = 
    let lst = String.split_on_char '\n' str in 
    let lst = List.map String.length lst in 
    let lst = List.fold_left (fun init e -> 
      let (_, _end) = List.hd init in 
      (_end, _end + 1 + e) :: init
      ) [(0,0)] lst  
    in 
    (* Adapt the first and last element *)
    let (_begin, _end) = List.hd lst in
    let lst = List.tl (List.rev lst) in 
  
    fun (pos1, pos2) -> 
      let rec _inner pos ranges depth =
        match ranges with 
        | (x, y) :: rest -> 
          if x <= pos && y > pos then 
            (depth, pos - x) 
          else 
            _inner pos rest (depth + 1)
        | _ -> raise (Invalid_argument "Position outside range")
      in
      let (l1, c1) = _inner pos1 lst 0 in 
      let (l2, c2) = _inner pos2 lst 0 in 
      (l1, c1, l2, c2) *)
end 

include Gramm_

let is_closed (g : t) : bool = 
  let module StringSet = Set.Make (String) in 
  let find_all_refs (rs : Rule.t list) : StringSet.t = 
    let refs_in_p_exprs set = function 
      | Parse_expr.Reference s -> StringSet.add s set 
      | _ -> set 
    in 
    let refs_in_choice set Choice.{ symbols = symbs; _ } = 
      let (_, p_exprs) = List.split symbs in 
      List.fold_left refs_in_p_exprs set p_exprs 
    in 
    let refs_in_rule set rl = 
      List.fold_left refs_in_choice set rl 
    in 
    List.fold_left refs_in_rule StringSet.empty rs 
  in 
  let (names, rules) = List.split g.rules in 
  let name_set = StringSet.of_list names in 
  let rule_set = find_all_refs rules in 
  let diff = StringSet.diff rule_set name_set in 
  StringSet.is_empty diff
