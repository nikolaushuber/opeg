module G = Grammar 

let ( let* ) o f = match o with 
  | Some x -> f x 
  | None -> None 

type pattern = 
  | Ref of string
  | RegEx of Re.re 

type alt = pattern list 

type frame = alt list 

type state = {
  str : string; 
  len : int; 
  mutable pos : int; 
  mutable stack : frame list; 
  grammar : G.t; 
  frame_tbl : (string, frame) Hashtbl.t; 
}

let make_frame_dict (g : G.t) = 
  let ws_regex = 
    RegEx (Re.Perl.compile_pat ("[" ^ String.concat "|" g.whitespace ^ "]?"))
  in 

  let pattern_to_pattern (p : G.pattern) : pattern = 
    match p.ty with 
    | Reference (name, _) -> Ref name 
    | Regex rx -> RegEx (Re.Perl.compile_pat rx)
    | Quote name -> RegEx (Re.compile (Re.str name))
    | _ -> failwith "Pattern not yet implemented" 
  in
  
  let choice_to_alt ?(lex = false) (c : G.choice)  : alt = 
    let pat_list = List.map pattern_to_pattern c.patterns in 
    List.rev (if lex then 
      pat_list 
    else 
      (* If this is not a lexing rule there can be whitespace inbetween any symbol *)
      ws_regex :: List.fold_left 
      (fun init pat -> pat :: ws_regex :: init) [] pat_list
    )
  in 

  let rule_to_frame ?(lex = false) (r : G.rule) : frame = 
    List.map (choice_to_alt ~lex:lex) r.choices 
  in 

  let num_rules = List.length g.lexing + List.length g.rules in 
  let tbl = Hashtbl.create num_rules in 
  List.iter (fun (r : G.rule) -> Hashtbl.add tbl r.name (rule_to_frame ~lex:true r)) g.lexing; 
  List.iter (fun (r : G.rule) -> Hashtbl.add tbl r.name (rule_to_frame r)) g.rules; 
  tbl 

let init_interp (g : G.t) (str : string) = 
  {
    str = str; 
    len = String.length str; 
    pos = 0 ;
    grammar = g; 
    stack = [[[Ref g.start]]]; 
    frame_tbl = make_frame_dict g; 
  }


let try_regex regx curr : string option =
  try  
    let ret = Re.exec ~pos:curr.pos regx curr.str in  
    let (_, len) = Re.Group.offset ret 0 in 
    curr.pos <- len; 
    Some (Re.Group.get ret 0)
  with 
    Not_found -> None 
     
(* We use this for creating the stack on a failure to parse *)
let rec create_failed_stack (alts : alt list) (frames : frame list) = 
  match alts with 
  | _ :: _ -> alts :: frames 
  | [] -> begin 
    match frames with 
    | fr :: rest -> create_failed_stack fr rest 
    | _ -> []
  end 

let step (curr : state) : unit = 
  (* Get the uppermost frame in the current stack *)
  match curr.stack with 
  | fr :: other_frms -> begin 
    (* Get the uppermost alternative *) 
    match fr with 
    | alt :: other_alts -> begin 
      (* Get the uppermost symbol from the current alternative *)
      match alt with 
      | sym :: other_syms -> begin 
        match sym with 
          | Ref name -> begin 
              (* We need to create a new frame *)
              let new_fr = Hashtbl.find curr.frame_tbl name in 
              curr.stack <- new_fr :: (other_syms :: other_alts) :: other_frms 
            end 
          | RegEx regx -> begin 
            match try_regex regx curr with 
            (* Found something so continue with rest of the symbols in the current 
               alternative 
            *)
            | Some _ -> curr.stack <- (other_syms :: other_alts) :: other_frms 
            (* Found nothing, so continue with other alternative *)
            | None -> curr.stack <- create_failed_stack other_alts other_frms 
          end
      end 
      (* There are no more symbols in the current alternative, so continue in next frame *)
      | [] -> curr.stack <- other_frms 
    end 
    (* No more alternatives *)
    | [] -> curr.stack <- other_frms  
  end 
  (* No more frames *)
  | [] -> () 

let rec parse (s : state) : bool = 
  step s; 
  if List.length s.stack == 0 then 
    (if s.pos == s.len then true else false)
  else 
    parse s 
