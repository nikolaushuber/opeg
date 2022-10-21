module MyServer (R : Idl.RPC) = struct 
  open R 
  open Idl 

  let description = Interface.{
    name = "Opeg"; 
    namespace = None; 
    description = ["This is the opeg RPC interface"]; 
    version = 1, 0, 0; 
  }

  let implementation = implement description 

  let i = Param.mk Rpc.Types.int 
  let e1 = Idl.DefaultError.err 

  let add = declare "add" [] (i @-> i @-> returning i e1) 
end 

module M = Idl.IdM  

module MyIdl = Idl.Make (M) 

(* module Client = MyServer (MyIdl.GenClient ())  *)
module Server = MyServer (MyIdl.GenServer ()) 

let _ = 
  Printf.fprintf Out_channel.stdout "%s\n%!" "Server started!"; 
  Server.add (fun a b -> MyIdl.ErrM.return (a+b)); 

  let rpc_func = MyIdl.server Server.implementation in 
  
  let rec loop _ = 
    let open M in 
    match In_channel.input_line In_channel.stdin with 
    | None -> loop () 
    | Some inp ->
      if String.equal "" inp then loop () else 
      let _, id, call = Jsonrpc.version_id_and_call_of_string inp in 
      Printf.printf "%s\n" (Rpc.string_of_call call); 
      rpc_func call >>= fun res -> Jsonrpc.string_of_response ~id res |> return 
      >>= fun res -> Printf.fprintf Out_channel.stdout "%s\n%!" res;
      loop ()
  in 
  loop () 

