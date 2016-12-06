open Lwt.Infix
open Result
open Protocol_9p

let src = Logs.Src.create ~doc:"logger for datakit entrypoint" "osilo.silo"
module Log = (val Logs.src_log src : Logs.LOG)

module Client : sig
  type t
  exception Failed_to_make_silo_client of Uri.t
  val create : server:string -> t
  val server : t -> string
  module Silo_9p_client : sig
    include (module type of Client9p_unix.Make(Log))
  end
  module Silo_datakit_client : sig
    include (module type of Datakit_client_9p.Make(Silo_9p_client))
  end
end = struct
  type t = {
    server : string (* Silo_9p_client.connect takes a string not a Uri.t *)
  }

  exception Failed_to_make_silo_client of Uri.t

  let create ~server =
    let address = Printf.sprintf "%s:5640" server in
    Log.info (fun m -> m "Creating silo client for Datakit server at %s" address);
    { server = address }

  let server c = c.server

  module Silo_9p_client = Client9p_unix.Make(Log)
  
  module Silo_datakit_client = Datakit_client_9p.Make(Silo_9p_client)
end
  
exception Checkout_failed
exception Write_failed
exception Read_failed

open Client.Silo_datakit_client

let connect client =
  Client.Silo_9p_client.connect "tcp" (Client.server client) () 
  >|= begin function
      | Ok conn_9p  -> (conn_9p,(conn_9p |> connect))
      | Error error -> raise Checkout_failed
      end

let disconnect conn_9p conn_dk =
  disconnect conn_dk
  >>= fun () -> Client.Silo_9p_client.disconnect conn_9p

let checkout service conn_dk = 
  branch conn_dk service 
  >|= begin function
      | Ok branch   -> branch
      | Error error -> raise Checkout_failed
      end

let write ~client ~peer ~service ~contents =
  let content = 
    match contents with
    | `Assoc l -> l
    | _        -> raise Write_failed
  in connect client
  >>= fun (c9p,cdk) -> 
    (checkout service cdk
     >>= (fun branch -> 
       Branch.transaction branch 
       >|= begin function 
           | Ok tr   -> tr
           | Error r -> raise Write_failed
           end 
       >>= fun tr ->
         (let write_file (f,c) =
            let c' = Yojson.Basic.to_string c |> Cstruct.of_string in
            Transaction.create_or_replace_file tr (Datakit_path.of_string_exn f) c' 
            >|= begin function
                | Ok ()   -> ()
                | Error e -> raise Write_failed
                end
          in
            (try 
               Lwt_list.iter_s write_file content
               >>= fun () -> (Transaction.commit tr ~message:"Write to silo")
             with 
             | Write_failed -> Transaction.abort tr >|= fun () -> Ok ()))
     >>= begin function
         | Ok () -> Log.info (fun m -> m "Disconnecting"); disconnect c9p cdk
         | Error (`Msg msg) -> Log.info (fun m -> m "%s" msg); raise Write_failed 
         end))

let read ~client ~peer ~service ~files =
  let branch = Printf.sprintf "%s" service in
  connect client
  >>= fun (c9p,cdk) -> 
    (checkout service cdk
     >>= fun branch -> Client.Silo_datakit_client.Branch.head branch
     >|= begin function 
         | Ok ptr      -> ptr
         | Error error -> raise Read_failed
         end
     >|= begin function
         | Some head -> head
         | None      -> raise Read_failed
         end
     >|= Client.Silo_datakit_client.Commit.tree
     >>= fun tree ->
       let f file = 
         Client.Silo_datakit_client.Tree.read_file tree (Datakit_path.of_string_exn file)
         >|= begin function
             | Ok cstruct  -> (Printf.sprintf "%s" file),(cstruct |> Cstruct.to_string |> Yojson.Basic.from_string)
             | Error error -> (Printf.sprintf "%s" file),`Null
             end
       in
         ((Lwt_list.map_s f files)
         >|= (fun l -> (`Assoc l)) 
         >>= fun r -> (disconnect c9p cdk >|= fun () -> r)))
