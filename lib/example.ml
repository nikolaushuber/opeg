module rec A : sig 
  type t = B.t
  val eval : B.t -> int 
end = struct 
  type t = B.t 
  let eval b = b.B.x 
end

and B : sig 
  type t = {
    x : int; 
    y: int; 
  }
end = struct 
  type t = {
    x : int; 
    y : int; 
  }
end 
