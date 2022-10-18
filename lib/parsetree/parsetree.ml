module Json = Parsetree_j 

include Parsetree_t 

let pp_ptree (fmt : Format.formatter) (n : node) : unit = 
  let rec _inner (depth : int) (n : node)  = 
    let pre = match n.ref with 
      | Some n -> n ^ " = " 
      | None -> "" 
    in 

    let (b, e) = n.pos in 

    (if depth > 0 then "|" ^ String.make depth '-' ^ "> " else "") ^ pre ^ n.name ^ "." ^ n.choice ^ 
    " @ (" ^ string_of_int b ^ "," ^ string_of_int e ^ ")" ^
    begin 
      match n.content with 
      | Lexeme l -> ": \"" ^ l ^ "\"" 
      | Node n' -> "\n" ^ String.concat "\n" (List.map (_inner (depth + 1)) n') 
    end 
    
  in 

  Format.fprintf fmt "%s" (_inner 0 n) 

