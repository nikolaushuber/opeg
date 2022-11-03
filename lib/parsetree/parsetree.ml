module Json = Parsetree_j 

include Parsetree_t 

(*
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
      | Hole -> "<?>" 
    end 
    
  in 

  Format.fprintf fmt "%s" (_inner 0 n)  *)

let is_same_tree (p1 : t) (p2 : t) = 
  let same_name = match p1.name, p2.name with 
    | None, None -> true 
    | Some x, Some y when String.equal x y -> true 
    | _ -> false 
  in 
  String.equal p1.rule p2.rule && 
  Int.equal p1.choice p2.choice && 
  Int.equal (fst p1.pos) (fst p2.pos) && 
  Int.equal (snd p1.pos) (snd p2.pos) && 
  same_name

let rec simplify (p : t) : t = 
  match p.node with 
  | Tree s -> begin 
    match s with 
    | x :: [] when is_same_tree p x -> {p with node = x.node}
    | _ -> {p with node = Tree (List.map simplify s)}
  end 
  | _ -> p 

let to_string (p : t) : string = 
  let rec to_string (p : t) : string = 
    match p.node with 
    | Lexeme s -> s 
    | Option None -> "" 
    | Option (Some o) -> to_string o 
    | Tree s -> String.concat "" (List.map to_string s)
  in to_string (simplify p) 

let pp_ptree (fmt : Format.formatter) (n : t) : unit = 
  Format.fprintf fmt "%s" (Json.string_of_t n) 
