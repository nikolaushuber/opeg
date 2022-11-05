open Lib

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

  let s = Param.mk Rpc.Types.string 
  let i = Param.mk Rpc.Types.int 
  
  type grammar_item = (string * bool * bool)
  [@@deriving rpcty]

  type grammar_item_list = grammar_item list 
  [@@deriving rpcty]

  let grammar_list_p = Param.mk grammar_item_list

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

  let parse = declare "parse" [] (s @-> s @-> returning s error)
  let add = declare "add" [] (i @-> i @-> returning i Idl.DefaultError.err)
  let get_grammar_list = declare "get_grammar_list" [] (s @-> returning grammar_list_p Idl.DefaultError.err)
end 

(* Use standard id monad *)
module OpegIdl = Idl.Make (Idl.IdM)
module Server = OpegServer (OpegIdl.GenServer ()) 

let _ = 
  Server.parse (fun grammar_str input -> 
    let g = Grammar_utils.string_to_grammar grammar_str in 
    try 
      let res = Interpreter.interpret g input in 
      OpegIdl.ErrM.return (Parsetree.Json.string_of_t res)
    with 
      | Failure _ -> OpegIdl.ErrM.return_err (Server.Parse_error "Error during parsing")
  ); 

  Server.get_grammar_list (fun str -> 
    let grammars = Grammar_utils.string_to_grammar_list str in 
    let grammar_items = List.map (fun (n, g) -> (n, false, Lib.Grammar.is_closed g)) grammars in 
    OpegIdl.ErrM.return (grammar_items)
  );

  Server.add (fun a b -> OpegIdl.ErrM.return (a + b)); 

  let rpc_func = OpegIdl.server Server.implementation in 

  (* Let the magic start here *)
  let socket_str = "tcp://*:5555" in 
  let context = Zmq.Context.create () in 
  let socket = Zmq.Socket.create context Zmq.Socket.rep in 
  Zmq.Socket.bind socket socket_str;
  print_endline ("Opeg Server started @ " ^ socket_str);  

  let rec loop _ = 
    let open Idl.IdM in 
    (* Still fascinated that that works *)
    let inp = Zmq.Socket.recv socket in 
    print_endline ("Received: " ^ inp); 
    let _, id, call = Jsonrpc.version_id_and_call_of_string inp in 
    rpc_func call >>= fun res -> 
      let res_str = Jsonrpc.string_of_response ~id res in 
      print_endline ("Send: " ^ res_str); 
      res_str |> return 
    (* Look, some more magic *)
    >>= fun res -> Zmq.Socket.send socket res; 
    loop () 
  in 
  loop () 
 