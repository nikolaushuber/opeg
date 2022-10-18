module G = Grammar 
module P = Parsetree

type pattern = 
  | Ref of string option * string
  | RegEx of string option * string * Re.re

type choice = {
  name : string; 
  symbols : pattern list
}

type frame = {
  name : string;
  choices : choice list;
  start : int;
  results : P.node list; 
  ref : string option; 
  curr_pos : int; 
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
    RegEx (None, "#whitespace", (Re.Perl.compile_pat ("[" ^ String.concat "|" g.whitespace ^ "]?")))
  in 

  let pattern_to_pattern (p : G.pattern) : pattern = 
    match p.ty with 
    | Reference (name, _) -> Ref (p.ref, name) 
    | Regex rx -> RegEx (p.ref, rx, (Re.Perl.compile_pat rx))
    | Quote name -> RegEx (p.ref, name, (Re.compile (Re.str name)))
    | _ -> failwith "Pattern not yet implemented" 
  in
  
  let choice_to_alt ?(lex = false) (c : G.choice) = 
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
      results = []; 
      ref = None; 
      curr_pos = 0; 
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
        name = "#start";  
        choices = [
          {name = "#start"; symbols = [Ref (None, g.start)]}
        ];
        start = 0; 
        results = []; 
        ref = None; 
        curr_pos = 0; 
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
        (
          s.stack <- rest; 
          backtrack s
        )
      else 
      (
        (* Delete uppermost choice and reset results list *)
        s.stack <- {fr with choices = other_choices; curr_pos = fr.start; results = []} :: rest; 
        s.pos <- fr.start;
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
          | Ref (ref, name) -> begin 
              (* We need to create a new frame *)
              let new_fr = {(Hashtbl.find curr.frame_tbl name) with start = curr.pos; curr_pos = curr.pos; ref = ref} in 
              curr.stack <- new_fr :: {fr with choices = {alt with symbols = other_syms} :: other_alts} :: other_frms 
          end 
          | RegEx (ref, n, regx) -> begin  
              let b_pos = curr.pos in 
              match try_regex regx curr with 
              (* Found something so continue with rest of the symbols in the current 
                alternative 
              *)
              | Some res -> 
                if String.equal "#whitespace" n then begin
                curr.stack <- 
                  {fr with 
                    choices = {alt with symbols = other_syms } :: other_alts;
                    curr_pos = b_pos + String.length res; 
                  } :: other_frms
              end else begin 
                curr.stack <- 
                {fr with 
                  choices = {alt with symbols = other_syms } :: other_alts;
                  curr_pos = b_pos + String.length res; 
                  results = {
                      name = fr.name; 
                      choice = alt.name; 
                      ref = ref; 
                      pos = (b_pos, b_pos + String.length res); 
                      content = Lexeme res;  
                    } :: fr.results;
                } :: other_frms
              end 
              (* Found nothing, so continue with other alternative *)
              | None -> backtrack curr
          end
      end 
      (* There are no more symbols in the current alternative, so continue in next frame *)
      | [] -> begin 
        (* Every frame but the first comes from an epansion *)
        match other_frms with 
        | first :: rest -> 
          (* Are there already results, if so we need to use their last position *)
          curr.stack <- {first with results = {
          name = fr.name; 
          choice = alt.name; 
          ref = fr.ref; 
          pos = (fr.start, curr.pos);
          content = Node (List.rev (fr.results)); 
        } :: first.results } :: rest; 
        | [] -> failwith "Error"
        end
    end 
    (* No more alternatives *)
    | [] -> curr.stack <- other_frms  
  end 
  (* No more frames *)
  | [] -> () 

let rec parse (s : state) : P.node = 
  step s; 
  if List.length s.stack == 1 then 
    (if s.pos == s.len then (List.nth (List.hd s.stack).results 0) else raise (Failure "No parse"))
  else 
    parse s 

let pp_stack (fmt : Format.formatter) (s : stack) : unit = 
  let print_pattern = function 
    | Ref (_, s) -> "Ref( " ^ s ^ " )" 
    | RegEx (_, s, _) -> "Regex( \"" ^ s ^ "\" )" 
  in 

  let print_choice (c : choice) = 
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
