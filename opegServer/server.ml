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
  let b = Param.mk Rpc.Types.bool 
  
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
  let ping = declare "ping" [] (returning b Idl.DefaultError.err)
  let generate = declare "generate_code" [] (s @-> returning s error)
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
    let grammar_items = List.map (fun (n, g) -> (n, Bool.not (Int.equal (List.length g.Grammar.parts) 0), Lib.Grammar.is_closed g)) grammars in 
    OpegIdl.ErrM.return (grammar_items)
  );

  Server.ping (OpegIdl.ErrM.return (true)); 

  Server.add (fun a b -> OpegIdl.ErrM.return (a + b)); 

  Server.generate (fun str -> 
    let g = Grammar_utils.string_to_grammar str in 
    let fmt = Format.str_formatter in 
    let fmt_funcs = Format.pp_get_formatter_out_functions fmt () in 
    let fmt_funcs = {fmt_funcs with 
      out_indent = (fun n -> fmt_funcs.out_string (String.make (2*n) ' ') 0 (2*n))} 
    in 
    let fmt = Format.formatter_of_out_functions fmt_funcs in 
    Gen.gen_grammar fmt g; 
    let out = Format.flush_str_formatter () in 
    OpegIdl.ErrM.return out 
  );

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
    print_endline ("Received: " ^ inp ^ "\n"); 
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
 