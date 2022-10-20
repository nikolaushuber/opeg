type json = Yojson.Basic.t

let jsonrpc_version = "2.0"

exception Parse_error

module Id = struct 
  type t = [
      `Int of int 
    | `String of string 
    | `Null
  ]

  let of_int i = `Int i 
  let of_str s = `String s 
  let of_null _ = `Null 

  let to_json (id : t) : json = match id with 
    | `Int _ as i -> i 
    | `String _ as s -> s 
    | `Null -> `Null 

  let of_json (j : json) : t = match j with 
  | `Int _ as i -> i 
  | `String _ as s -> s 
  | `Null -> `Null 
  | _ -> raise Parse_error
end 

module Request = struct 
  type t = {
    method_ : string; 
    params : json option; 
    id : Id.t option; 
  }

  let is_notification (r : t) : bool = Option.is_none r.id 

  let make method_ params id : t =
    { method_ = method_; params = params; id = id }

  let to_json (r : t) : json = `Assoc (
    [
      ("jsonrpc", `String jsonrpc_version); 
      ("method", `String r.method_) 
    ] 
    @ if Option.is_some r.id then 
      [("id", Id.to_json (Option.get r.id))] 
    else 
      []
    @ if Option.is_some r.params then 
      [("params", Option.get r.params)] 
    else 
      []
  ) 

  let of_json (j : json) : t = match j with 
  | `Assoc lst -> begin 
    let method_ = match List.assoc_opt "method" lst with 
      | Some (`String s) -> s 
      | _ -> raise Parse_error 
    in
    let params = List.assoc_opt "params" lst in 
    let id = match List.assoc_opt "id" lst with 
      | Some (`Int i) -> Some (`Int i)
      | Some (`String s) -> Some (`String s)
      | Some (`Null) -> Some (`Null)
      | None -> None 
      | _ -> raise Parse_error  
    in
    {
      method_ = method_; 
      params = params; 
      id = id; 
    }
  end 
  | _ -> raise Parse_error

  let of_string s = of_json (Yojson.Basic.from_string s)
end 

module Response = struct 
  module Error = struct 
    type t = {
      code : int; 
      message : string; 
      data : json option; 
    }
  
    let make c m d = {code = c; message = m; data = d}

    module Code = struct 
      let parse_error = -32700 
      let invalid_request = -32600 
      let method_not_found = -32601  
      let invalid_params = -32602  
      let internal_error = -32603  
    end 

    let to_json (err : t) : json = `Assoc ([
      ("code", `Int err.code); 
      ("message", `String err.message); 
    ] @ if Option.is_some err.data then [("data", Option.get err.data)] else [])

    let of_json (j : json) : t = match j with 
      | `Assoc lst -> begin 
        let code = match List.assoc_opt "code" lst with 
        | Some (`Int i) -> i 
        | _ -> raise Parse_error 
        in
        let message = match List.assoc_opt "message" lst with 
        | Some (`String s) -> s 
        | _ -> raise Parse_error 
        in
        let data = List.assoc_opt "data" lst in 
        {code = code; message = message; data = data}  
      end 
      | _ -> raise Parse_error 
    
    let of_string (s : string) : t = of_json (Yojson.Basic.from_string s)
  end 

  type typ = 
    | ErrorResponse of Error.t 
    | OkResponse of json  

  type t = {
    id : Id.t; 
    payload : typ; 
  }

  let to_json (r : t) : json = `Assoc ([
    ("jsonrpc", `String jsonrpc_version); 
    ("id", Id.to_json r.id); 
  ] @ match r.payload with 
    | ErrorResponse e -> [("error", Error.to_json e)]
    | OkResponse j -> [("result", j)]
  ) 

  let of_json (j : json) : t = match j with 
  | `Assoc lst -> begin 
    let id = match List.assoc_opt "id" lst with 
      | Some i -> Id.of_json i 
      | _ -> raise Parse_error 
    in
    let error = match List.assoc_opt "error" lst with 
      | Some e -> Some (ErrorResponse (Error.of_json e)) 
      | None -> None 
    in 
    let result = match List.assoc_opt "result" lst with 
      | Some j -> Some (OkResponse j) 
      | None -> None 
    in 
    let payload = match error, result with 
      | Some e, None -> e 
      | None, Some r -> r 
      | _ -> raise Parse_error 
    in
    {id = id; payload = payload} 
  end 
  | _ -> raise Parse_error
end 

module Batch (
  M : sig
    type t 
    val to_json : t -> json 
    val of_json : json -> t 
  end
) = struct 
  type t = M.t list 

  let to_json l = `List (List.map M.to_json l) 
  let of_json l = match l with 
  | `List ls -> List.map M.of_json ls 
  | _ -> raise Parse_error 
end 

module ResponseBatch = Batch(Response)
module RequestBatch = Batch(Request)

module Server = struct 
  type callback = Request.t -> Response.t option 

  type t = {
    funcs : (string, callback) Hashtbl.t; 
    default : callback 
  }

  let add_func (s : t) (name : string) (cb : callback) = 
    Hashtbl.add s.funcs name cb 

  let rm_func (s : t) (name : string) = 
    Hashtbl.remove s.funcs name 

  let make () = {
    funcs = Hashtbl.create 10; 
    default = (fun _ -> raise Parse_error)
  }

  let find_func (s : t) (name : string) =  
    match Hashtbl.find_opt s.funcs name with 
    | Some f -> f 
    | None -> s.default 

  let eval (j : json) (s : t) : Response.t option = 
    let res = Request.of_json j in 
    let f = find_func s res.method_ in 
    let resp = f res in 
    match res.id with 
    | Some _ -> resp 
    | None -> None 
end 

module Function = struct 
  module Params = struct 
    type t = 
      | Int 
      | String 
      | Bool 
      | Json 
  end 

  let make 
    (name : string) 
    (params : (string * Params.t) list) 
    (f : 'a -> 'b) 
  : Request.t -> Response.t = 
    failwith "not implemented"

end 
