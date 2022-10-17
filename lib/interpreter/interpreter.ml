module G = Grammar 

type pattern = 
  | Ref of string
  | RegEx of string * Re.re 

type alt = {
  name : string; 
  symbols : pattern list
}

type frame = {
  name : string;
  choices : alt list;
  mutable start : int;
}

type stack = frame list 

type state = {
  str : string; 
  len : int; 
  mutable pos : int; 
  mutable stack : stack; 
  grammar : G.t; 
  frame_tbl : (string, frame) Hashtbl.t; 
}

let make_frame_dict (g : G.t) = 
  let ws_regex = 
    RegEx ("whitespace", (Re.Perl.compile_pat ("[" ^ String.concat "|" g.whitespace ^ "]?")))
  in 

  let pattern_to_pattern (p : G.pattern) : pattern = 
    match p.ty with 
    | Reference (name, _) -> Ref name 
    | Regex rx -> RegEx (rx, (Re.Perl.compile_pat rx))
    | Quote name -> RegEx (name, (Re.compile (Re.str name)))
    | _ -> failwith "Pattern not yet implemented" 
  in
  
  let choice_to_alt ?(lex = false) (c : G.choice)  : alt = 
    let pat_list = List.map pattern_to_pattern c.patterns in 
    {
      name = c.name; 
      symbols = List.rev (
        if lex then 
          pat_list 
        else 
          (* If this is not a lexing rule there can be whitespace inbetween any symbol *)
          ws_regex :: List.fold_left 
          (fun init pat -> pat :: ws_regex :: init) [] pat_list
        ); 
    }
  in 

  let rule_to_frame ?(lex = false) (r : G.rule) : frame = 
    {
      name = r.name; 
      choices = List.map (choice_to_alt ~lex:lex) r.choices; 
      start = 0; 
    }
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
    stack = [
      {
        name = "start";  
        choices = [
          {name = "start"; symbols = [Ref g.start]}
        ];
        start = 0; 
      }
    ]; 
    frame_tbl = make_frame_dict g; 
  }


let try_regex regx curr : string option =
  try  
    let ret = Re.exec ~pos:curr.pos regx curr.str in  
    let (start, len) = Re.Group.offset ret 0 in 
    if start == curr.pos then begin 
      curr.pos <- len; 
      Some (Re.Group.get ret 0)
    end else None
  with 
    Not_found -> None 
     
(* We use this for creating the stack on a failure to parse *)
let rec backtrack (s : state) : unit =  
  let stack = s.stack in 
  (* if there are no more frames left, just return *)
  match stack with 
  | [] -> ()
  | fr :: rest -> begin 
    match fr.choices with 
    (* Are there any choices left in the topmost frame? *)
    | _ :: other_choices ->  
      if List.length other_choices == 0 then 
        (s.stack <- rest; backtrack s)
      else 
      (
        s.stack <- {fr with choices = other_choices} :: rest; 
        s.pos <- fr.start
      )
    (* If there are no more choices, try again in the next frame *)
    | [] -> (
      s.stack <- rest; 
      backtrack s 
    )
  end 

let step (curr : state) : unit = 
  (* Get the uppermost frame in the current stack *)
  match curr.stack with 
  | fr :: other_frms -> begin 
    (* Get the uppermost alternative *) 
    match fr.choices with 
    | alt :: other_alts -> begin 
      (* Get the uppermost symbol from the current alternative *)
      match alt.symbols with 
      | sym :: other_syms -> begin 
        match sym with 
          | Ref name -> begin 
              (* We need to create a new frame *)
              let new_fr = Hashtbl.find curr.frame_tbl name in 
              new_fr.start <- curr.pos; 
              curr.stack <- new_fr :: {fr with choices = {alt with symbols = other_syms} :: other_alts} :: other_frms 
            end 
          | RegEx (_, regx) -> begin  
            match try_regex regx curr with 
            (* Found something so continue with rest of the symbols in the current 
               alternative 
            *)
            | Some _ -> curr.stack <- {fr with choices = {alt with symbols = other_syms} :: other_alts} :: other_frms 
            (* Found nothing, so continue with other alternative *)
            | None -> backtrack curr
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

let pp_stack (fmt : Format.formatter) (s : stack) : unit = 
  let print_pattern = function 
    | Ref s -> "Ref( " ^ s ^ " )" 
    | RegEx (s, _) -> "Regex( \"" ^ s ^ "\" )" 
  in 

  let print_choice (c : alt) = 
    if List.length c.symbols == 0 then "" else 
    "| -> " ^ String.concat "\n| -> " (List.map print_pattern c.symbols)
  in 

  let print_frame (fr : frame) = 
    "( " ^ fr.name ^ " @ " ^ string_of_int fr.start ^ " )\n" ^   
    "---------------------------------------\n" ^ 
    (if List.length fr.choices == 0 then "" else 
    String.concat "\n| - - - - - - - - - - - - - - - - \n" (List.map print_choice fr.choices) ^ "\n") ^ 
    "---------------------------------------\n"
  in 

  let str = String.concat "\n" (List.map print_frame s) in 
  Format.fprintf fmt "%s" str 
    
