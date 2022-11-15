type 'a parse_result =
  | Parse of 'a
  | No_parse

type state = {
  input : string;
  mutable pos : int
}

let make_state input = {
  input = input; 
  pos = 0; 
}

let ( let* ) o f = match o with 
  | Parse p -> f p 
  | No_parse -> No_parse 

let expect_eof (s : state) : unit parse_result = 
  if s.pos == String.length s.input then 
    Parse () 
  else
    No_parse 

let try_in_seq alts (s : state) = 
  let curr = s.pos in 
  let rec _inner alts = match alts with 
  | [] -> No_parse
  | alt :: rest -> begin 
    match alt s with 
    | Parse p -> Parse p 
    | No_parse -> (s.pos <- curr; _inner rest) 
  end 
  in
  _inner alts

let parse_string (s : state) (str : string) = 
  try 
    let sub = String.sub s.input s.pos (String.length str) in 
    if (String.starts_with ~prefix:str sub) then 
      let len = String.length str in 
      s.pos <- s.pos + len; 
      Parse str 
    else 
      No_parse 
  with 
    | _ -> No_parse 

let parse_regex (s : state) (r : string) = 
  let rx = Re.Perl.compile_pat r in 
  try 
    let ret = Re.exec ~pos:s.pos rx s.input in 
    let (start, len) = Re.Group.offset ret 0 in 
    if Int.equal start s.pos then begin 
      s.pos <- len; 
      Parse (Re.Group.get ret 0) 
    end else
      No_parse 
  with
    | Not_found -> No_parse  
