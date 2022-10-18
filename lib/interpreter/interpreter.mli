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

val init_interp : G.t -> string -> state

val step : state -> unit 

val parse : state -> P.node 

val pp_stack : Format.formatter -> stack -> unit 
[@@ocaml.toplevel_printer]
  