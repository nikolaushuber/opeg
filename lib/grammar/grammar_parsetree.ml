type toplevel_def = 
  | Grammar_def of string option * Grammar.t 
  | Composite_def of string option * string list 
