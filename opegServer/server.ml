module OpegServer (R : Idl.RPC) = struct 
  open R 
  open Idl 

  let description = Interface.{
    name = "OpegServer"; 
    namespace = None; 
    description = ["Json-RPC interface for the Opeg framework"]; 
    version = 1, 0, 0; 
  }

  let implementation = implement description 

  let grammar = Param.mk ~name:"grammar" ~description:["Input grammar"] Rpc.Types.string 
  let input = Param.mk ~name:"input" ~description:["Input to parse"] Rpc.Types.string 
  let parsetree = Param.mk ~name:"parsetree" ~description:["Parsetree"] Rpc.Types.string 

  type parse_error = 
  | Grammar_error of string 
  | Parse_error of string 
  | Unexpected_error of string 
  [@@deriving rpcty] 

  module E = Idl.Error.Make (struct 
    type t = parse_error
    let t = parse_error 
    let internal_error_of e = Some (Unexpected_error (Printexc.to_string e))
  end)

  let error = E.error 

  let parse = declare "parse" [] (grammar @-> input @-> returning parsetree error)
end 

(* Use standard id monad *)
module OpegIdl = Idl.Make (Idl.IdM)
module Server = OpegServer (OpegIdl.GenServer ()) 

let _ = 
  Server.parse (fun grammar input -> 
    let lbuf = Lexing.from_string grammar in 
    let g = Parser.start Lexer.read_token lbuf in 
    let state = Interpreter.init_interp g input in 
    try 
      let res = Interpreter.parse state in 
      OpegIdl.ErrM.return (Parsetree.Json.string_of_node res)
    with 
      | Failure _ -> OpegIdl.ErrM.return_err (Server.Parse_error "Error during parsing")
  ); 

  let rpc_func = OpegIdl.server Server.implementation in 

  let rec loop _ = 
    let open Idl.IdM in 
    match In_channel.input_line In_channel.stdin with 
    | None -> loop () 
    | Some inp -> 
      if String.equal "" inp then loop () else 
      let _, id, call = Jsonrpc.version_id_and_call_of_string inp in 
      rpc_func call >>= fun res -> Jsonrpc.string_of_response ~id res |> return 
      >>= fun res -> Printf.fprintf Out_channel.stdout "%s\n%!" res; 
      loop () 
    in 
    loop () 
