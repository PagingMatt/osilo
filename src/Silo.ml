open Lwt.Infix
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
    server : string
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
  
exception Checkout_failed of string * string
exception Connection_failed of string * string
exception Cannot_get_head_commit of string * string
exception Cannot_create_transaction of string * string
exception No_head_commit of string
exception Create_or_replace_file_failed of string
exception Cannot_create_parents of string * string
exception Write_failed of string
exception Delete_failed of string
exception Delete_file_failed of string

open Client.Silo_datakit_client

let connect client =
  Client.Silo_9p_client.connect "tcp" (Client.server client) () 
  >|= begin function
      | Ok conn_9p  -> (conn_9p,(conn_9p |> connect))
      | Error (`Msg msg) -> raise (Connection_failed ((Client.server client), msg))
      end

let disconnect conn_9p conn_dk =
  disconnect conn_dk
  >>= fun () -> Client.Silo_9p_client.disconnect conn_9p

let checkout service conn_dk = 
  branch conn_dk service 
  >|= begin function
      | Ok branch -> branch
      | Error (`Msg msg) -> raise (Checkout_failed (service, msg))
      end

let walk_path_exn p tr =
  let path = Datakit_path.of_string_exn p in
  match Core.Std.String.split ~on:'/' p with
  | []    -> Lwt.return path
  | x::[] -> Lwt.return path
  | path' -> 
      Core.Std.List.take path' ((List.length path') - 1) 
      |> String.concat "/" 
      |> Datakit_path.of_string_exn
      |> Transaction.make_dirs tr
      >|= begin function
          | Ok ()            -> path
          | Error (`Msg msg) -> raise (Cannot_create_parents (p,msg))
          end

let build_branch ~peer ~service =
  Printf.sprintf "%s/%s" (Peer.host peer) service

let write ~client ~peer ~service ~contents =
  let content = 
    match contents with
    | `Assoc l -> l
    | _        -> raise (Write_failed "Invalid file content.")
  in 
  if content = [] then Lwt.return () else 
  connect client
  >>= fun (c9p,cdk) -> 
    Log.debug (fun m -> m "Connected to Datakit server.");
    (checkout (build_branch ~peer ~service) cdk
     >>= (fun branch ->
       Log.debug (fun m -> m "Checked out branch %s" service);
       Branch.transaction branch 
       >|= begin function 
           | Ok tr -> Log.debug (fun m -> m "Created transaction on branch %s." service); tr
           | Error (`Msg msg) -> raise (Cannot_create_transaction (service, msg))
           end 
       >>= fun tr ->
         (let write_file (f,c) =
            let c' = Yojson.Basic.to_string c |> Cstruct.of_string in
            walk_path_exn f tr
            >>= (fun p -> Transaction.create_or_replace_file tr p c')
            >|= begin function
                | Ok ()   -> ()
                | Error (`Msg msg) -> raise (Create_or_replace_file_failed msg)
                end
          in
            (try
               Lwt_list.iter_s write_file content
               >>= fun () -> 
                 Log.debug (fun m -> m "Committing transaction."); 
                 (Transaction.commit tr ~message:"Write to silo")
             with 
             | Create_or_replace_file_failed msg -> 
                 Log.info (fun m -> m "Aborting transaction.\n%s" msg); 
                 Transaction.abort tr >|= fun () -> Ok ()))
     >>= begin function
         | Ok () -> 
             (Log.debug (fun m -> m "Disconnecting from %s" (Client.server client)); 
             disconnect c9p cdk
             >|= fun () -> Log.debug (fun m -> m "Disconnected from %s" (Client.server client)))
         | Error (`Msg msg) -> 
             (Log.err (fun m -> m "Aborted transaction: %s" msg); 
              Log.debug (fun m -> m "Disconnecting from %s" (Client.server client)); 
             disconnect c9p cdk
             >|= fun () -> 
               Log.debug (fun m -> m "Disconnected from %s" (Client.server client)); 
               raise (Write_failed msg))
         end))

let rec read_path tree acc path =
  Client.Silo_datakit_client.Tree.read tree (Datakit_path.of_string_exn path)
  >>= begin function
      | Ok t  ->
          (match t with
          | `File cstruct ->
              Lwt.return ((path,(cstruct |> Cstruct.to_string |> Yojson.Basic.from_string))::acc)
          | `Dir paths -> 
              (Lwt_list.fold_left_s (read_path tree) acc paths)
          | _ -> 
              Lwt.return ((path,`Null)::acc))
      | Error error -> Lwt.return ((path,`Null)::acc)
      end

let read ~client ~peer ~service ~files =
  if files = [] then Lwt.return (`Assoc []) else
  connect client
  >>= fun (c9p,cdk) -> 
    (checkout (build_branch ~peer ~service) cdk
     >>= fun branch -> Client.Silo_datakit_client.Branch.head branch
     >|= begin function 
         | Ok ptr      -> ptr
         | Error (`Msg msg) -> raise (Cannot_get_head_commit (service, msg))
         end
     >>= begin function
         | None      -> Lwt.return (`Assoc (Core.Std.List.map files ~f:(fun file -> (file,`Null))))
         | Some head -> 
            (let tree = Client.Silo_datakit_client.Commit.tree head in
              (Lwt_list.fold_left_s (read_path tree) [] files)
              >|= (fun l -> (`Assoc l)))
          end
      >>= fun r -> (disconnect c9p cdk >|= fun () -> r))

let delete ~client ~peer ~service ~files =
  if files = [] then Lwt.return () else
  connect client
  >>= fun (c9p,cdk) -> 
    (checkout (build_branch ~peer ~service) cdk
     >>= (fun branch ->
       Log.debug (fun m -> m "Checked out branch %s" service);
       Branch.transaction branch 
       >|= begin function 
           | Ok tr -> Log.debug (fun m -> m "Created transaction on branch %s." service); tr
           | Error (`Msg msg) -> raise (Cannot_create_transaction (service, msg))
           end 
       >>= fun tr ->
         (let delete_file f =
            Transaction.remove tr (Datakit_path.of_string_exn f)
            >|= begin function
                | Ok ()   -> ()
                | Error (`Msg msg) -> 
                    if msg = "No such file or directory" (* Should refactor to check exists on RO tree *)
                    then () else raise (Delete_file_failed msg)
                end
          in
            (try
               Lwt_list.iter_s delete_file files
               >>= fun () -> 
                 Log.debug (fun m -> m "Committing transaction."); 
                 (Transaction.commit tr ~message:"Delete files from silo")
             with 
             | Delete_file_failed msg -> 
                 Log.info (fun m -> m "Aborting transaction.\n%s" msg); 
                 Transaction.abort tr >|= fun () -> Error (`Msg msg)))
     >>= begin function
         | Ok () -> 
             (Log.debug (fun m -> m "Disconnecting from %s" (Client.server client)); 
             disconnect c9p cdk
             >|= fun () -> Log.debug (fun m -> m "Disconnected from %s" (Client.server client)))
         | Error (`Msg msg) -> 
             (Log.err (fun m -> m "Aborted transaction: %s" msg); 
              Log.debug (fun m -> m "Disconnecting from %s" (Client.server client)); 
             disconnect c9p cdk
             >|= fun () -> 
               Log.debug (fun m -> m "Disconnected from %s" (Client.server client)); 
               raise (Delete_failed msg))
         end))
