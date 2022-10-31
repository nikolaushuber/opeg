module P = Parsetree 

module Parse_result = struct
  type t =
    | Parse of int * P.t
    | No_parse
end

module rec Parse_expr : sig 
  type t = 
    | Match of Match_expr.t 
    | Predicate of Predicate_expr.t 
    | Repetition of Repetition_expr.t 
    | Reference of string 

  val eval : string -> Gramm_.t -> t -> int -> Parse_result.t
end = struct 
  type t = 
    | Match of Match_expr.t 
    | Predicate of Predicate_expr.t 
    | Repetition of Repetition_expr.t 
    | Reference of string 

  let eval inp g p pos : Parse_result.t = match p with 
    | Match m -> Match_expr.eval inp g m pos 
    | Predicate pred -> Predicate_expr.eval inp g pred pos 
    | Repetition rep -> Repetition_expr.eval inp g rep pos 
    | Reference r -> begin match List.assoc_opt r g.rules with 
      | Some rule -> Rule.eval inp g rule pos 
      | None -> failwith "Holes in grammar not yet supported" 
  end
end

and Repetition_expr : sig 
  type t = 
    | Zero_or_one of Parse_expr.t  
    | Zero_or_more of Parse_expr.t 
    | One_or_more of Parse_expr.t 

  val eval : string -> Gramm_.t -> t -> int -> Parse_result.t
end = struct 
  type t = 
    | Zero_or_one of Parse_expr.t  
    | Zero_or_more of Parse_expr.t 
    | One_or_more of Parse_expr.t 

  let receval inp g p pos = 
    let rec inner inp' p' pos' acc : Parsetree.t list = 
      match Parse_expr.eval inp' g p' pos' with 
      | Parse (next, tree) -> inner inp' p' next (tree :: acc)
      | No_parse -> List.rev acc 
    in 
  inner inp p pos []

  let eval inp g rep pos : Parse_result.t = match rep with 
    | Zero_or_one p -> begin 
      match Parse_expr.eval inp g p pos with 
      | Parse (next, tree) ->  
        Parse (next, P.mk tree.pos (Option (Some tree)) ) 
      | No_parse -> Parse (pos, P.mk (pos, pos) (Option None) ) 
    end
    | Zero_or_more p -> 
      let res = receval inp g p pos in 
      begin match res with
        | [] -> Parse (pos, P.mk (pos, pos) (List []) )
        | _ -> 
          let last = List.hd (List.rev res) in 
          Parse (snd last.pos, P.mk (pos, snd last.pos) (List res ))
      end 
    | One_or_more p -> 
      let res = receval inp g p pos in 
      begin match res with 
        | [] -> No_parse 
        | _ -> 
          let last = List.hd (List.rev res) in 
          Parse (snd last.pos, P.mk (pos, snd last.pos) (List res))
    end
end 

and Predicate_expr : sig
  type t = 
    | And of Parse_expr.t
    | Not of Parse_expr.t

  val eval : string -> Gramm_.t -> t -> int -> Parse_result.t
end = struct 
  type t = 
    | And of Parse_expr.t
    | Not of Parse_expr.t

  let eval inp g p pos : Parse_result.t = match p with 
    | And p' -> 
      begin match Parse_expr.eval inp g p' pos with 
      | Parse (_, tree) -> Parse (pos, tree) 
      | No_parse -> No_parse 
      end 
    | Not p' ->  
      begin match Parse_expr.eval inp g p' pos with 
      | Parse (_, _) -> No_parse
      | No_parse -> Parse (pos, P.mk (pos, pos) (Option None)) 
      end 
end

and Match_expr : sig 
  type t = 
    | Quote of string 
    | Regex of string 
  
  val eval : string -> Gramm_.t -> t -> int -> Parse_result.t 
end = struct
 type t = 
    | Quote of string 
    | Regex of string 
  
  let eval inp _ m pos : Parse_result.t = match m with 
    | Quote q -> begin 
      try 
        let sub = String.sub inp pos (String.length q) in 
        if (String.starts_with ~prefix:q sub) then 
          let len = String.length q in 
          let next = pos + len in 
          Parse (next, P.mk (pos, next) (Lexeme q))
        else 
          No_parse
      with
        | _ -> No_parse   
    end
    | Regex r -> begin 
      let rx = Re.Perl.compile_pat r in 
      try 
        let ret = Re.exec ~pos rx inp in 
        let (start, len) = Re.Group.offset ret 0 in 
        if Int.equal start pos then 
          Parse (len, P.mk (pos, len) (Lexeme (Re.Group.get ret 0)))
        else 
          No_parse
      with
        | Not_found -> No_parse
    end 
end

and Choice : sig 
  type t = {
    symbols : (string option * Parse_expr.t) list; 
    action : string; 
  }

  val eval : string -> Gramm_.t -> t -> int -> Parse_result.t 
end = struct 
  type t = {
    symbols : (string option * Parse_expr.t) list; 
    action : string; 
  }

  let eval inp g c pos : Parse_result.t = 
    let rec inner syms pos' acc : Parse_result.t = match syms with 
      | [] -> Parse (pos', P.mk (pos, pos') (List (List.rev acc)))
      | (name, p) :: xs -> 
        begin match Parse_expr.eval inp g p pos' with 
        | No_parse -> No_parse 
        | Parse (next, tree) -> inner xs next ({tree with name = name} :: acc)
        end
    in 
  inner c.symbols pos []
end

and Rule : sig 
  type t = Choice.t list 

  val eval : string -> Gramm_.t -> t -> int -> Parse_result.t 
end = struct 
  type t = Choice.t list

  let rec eval inp g r pos : Parse_result.t = match r with 
    | [] -> No_parse 
    | c :: cs -> begin match Choice.eval inp g c pos with 
      | Parse _ as p -> p 
      | No_parse -> eval inp g cs pos 
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

include Gramm_

module StringSet = Set.Make (String) 

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

let is_closed (g : t) : bool = 
  let (names, rules) = List.split g.rules in 
  let name_set = StringSet.of_list names in 
  let rule_set = find_all_refs rules in 
  let diff = StringSet.diff rule_set name_set in 
  StringSet.is_empty diff
