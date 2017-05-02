open Lwt.Infix
open Protocol_9p

let src = Logs.Src.create ~doc:"logger for datakit entrypoint" "osilo.silo"
module Log = (val Logs.src_log src : Logs.LOG)

exception Connection_failed of string * string
exception Datakit_error of string
exception Write_failed of string
exception Delete_failed

let (>>>=) fst snd =
  fst >>= function
  | Ok x                -> snd x
  | Error `Already_exists -> raise (Datakit_error "Target already exists.")
  | Error `Does_not_exist -> raise (Datakit_error "Target does not exist.")
  | Error `Is_dir         -> raise (Datakit_error "Attempting to use directory as a file.")
  | Error `Not_dir        -> raise (Datakit_error "Using a non-directory as a directory.")
  | Error `Not_file       -> raise (Datakit_error "Using a non-file as a file.")
  | Error `Not_symlink    -> raise (Datakit_error "Using non-symlink as a symlink.")
  | Error _ -> raise (Datakit_error "Unknown error.")

module Client : sig
  type t

  exception Failed_to_make_silo_client of Uri.t
  val create : server:string -> port:int -> t
  val address : t -> string

  module Silo_9p_client : sig
    include (module type of Client9p_unix.Make(Log))
  end
  module Silo_datakit_client : sig
    include (module type of Datakit_client_9p.Make(Silo_9p_client))
  end

  val connect :
    t -> (Silo_9p_client.t * Silo_datakit_client.t) Lwt.t
  val disconnect :
    Silo_9p_client.t      ->
    Silo_datakit_client.t -> unit Lwt.t
  val checkout :
    string                ->
    Silo_datakit_client.t -> Silo_datakit_client.Branch.t Lwt.t

  val new_transaction    :
    Silo_datakit_client.Branch.t ->
    Silo_datakit_client.Transaction.t Silo_datakit_client.or_error Lwt.t
  val commit_transaction :
    Silo_datakit_client.Transaction.t ->
    message:string -> unit Silo_datakit_client.or_error Lwt.t
  val abort_transaction  :
    Silo_datakit_client.Transaction.t -> unit Lwt.t

  val new_directory :
    Silo_datakit_client.Transaction.t ->
    Datakit_path.t -> unit Silo_datakit_client.or_error Lwt.t
  val new_file :
    Silo_datakit_client.Transaction.t ->
    Datakit_path.t ->
    Cstruct.t -> unit Silo_datakit_client.or_error Lwt.t
  val delete_file_or_directory :
    Silo_datakit_client.Transaction.t ->
    Datakit_path.t -> unit Silo_datakit_client.or_error Lwt.t
  val get_tree :
    Silo_datakit_client.Commit.t -> Silo_datakit_client.Tree.t
  val read_from_tree :
    Silo_datakit_client.Tree.t ->
    Datakit_path.t ->
    [ `Dir of string list | `File of Cstruct.t | `Link of string ]
      Silo_datakit_client.or_error Lwt.t
  val get_head_pointer :
    Silo_datakit_client.Branch.t ->
    Silo_datakit_client.Commit.t option Silo_datakit_client.or_error Lwt.t
end = struct
  type t = {
    server : string ;
    port   : int
  }

  exception Failed_to_make_silo_client of Uri.t

  let create ~server ~port =
    { server = server ; port = port ; }

  let address c = Printf.sprintf "%s:%d" c.server c.port

  module Silo_9p_client = Client9p_unix.Make(Log)

  module Silo_datakit_client = Datakit_client_9p.Make(Silo_9p_client)

  let connect client =
    Silo_9p_client.connect "tcp" (address client) ()
    >|= begin function
      | Ok conn_9p  -> (conn_9p,(conn_9p |> Silo_datakit_client.connect))
      | Error (`Msg msg) -> raise (Connection_failed ((address client), msg))
    end

  let disconnect conn_9p conn_dk =
    Silo_datakit_client.disconnect conn_dk
    >>= fun () -> Silo_9p_client.disconnect conn_9p

  let checkout service conn_dk =
    Silo_datakit_client.branch conn_dk service
    >>>= fun branch -> Lwt.return branch

  let new_transaction    = Silo_datakit_client.Branch.transaction
  let commit_transaction = Silo_datakit_client.Transaction.commit
  let abort_transaction  = Silo_datakit_client.Transaction.abort

  let new_directory = Silo_datakit_client.Transaction.make_dirs
  let new_file      = Silo_datakit_client.Transaction.create_or_replace_file
  let delete_file_or_directory = Silo_datakit_client.Transaction.remove
  let get_tree         = Silo_datakit_client.Commit.tree
  let read_from_tree   = Silo_datakit_client.Tree.read
  let get_head_pointer = Silo_datakit_client.Branch.head
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
    |> Client.new_directory tr
    >>>= fun () -> Lwt.return path

let build_branch ~peer ~service =
  Printf.sprintf "%s/%s" (Peer.string_of_t peer) service

let write ~client ~peer ~service ~contents =
  let content =
    match contents with
    | `Assoc l -> l
    | _        -> raise (Write_failed "Invalid file content.")
  in
  if content = [] then Lwt.return () else
    Client.connect client
    >>= fun (c9p,cdk) ->
    Log.debug (fun m -> m "Connected to Datakit server.");
    try
      (Client.checkout (build_branch ~peer ~service) cdk
       >>= (fun branch ->
           Log.debug (fun m -> m "Checked out branch %s" service);
           Client.new_transaction branch
           >>>= fun tr -> Log.debug (fun m -> m "Created transaction on branch %s." service);
           (let write_file (f,c) =
              let c' = Yojson.Basic.to_string c |> Cstruct.of_string in
              walk_path_exn f tr
              >>= (fun p -> Client.new_file tr p c')
              >>>= Lwt.return
            in
            (try
               Lwt_list.iter_s write_file content
               >>= fun () ->
               Log.debug (fun m -> m "Committing transaction.");
               (Client.commit_transaction tr ~message:"Write to silo")
             with
             | _ ->
               Log.info (fun m -> m "Aborting transaction.");
               Client.abort_transaction tr >|= fun () -> Ok ()))
           >>>= fun () ->
           (Log.debug (fun m -> m "Disconnecting from %s" (Client.address client));
            Client.disconnect c9p cdk
            >|= fun () -> Log.debug (fun m -> m "Disconnected from %s" (Client.address client)))))
    with _ -> Client.disconnect c9p cdk

let rec read_path tree acc path =
  Client.read_from_tree tree (Datakit_path.of_string_exn path)
  >>>= fun t ->
  (match t with
   | `File cstruct ->
     Lwt.return ((path,(cstruct |> Cstruct.to_string |> Yojson.Basic.from_string))::acc)
   | `Dir paths ->
     (Lwt_list.fold_left_s (read_path tree) acc
        (Core.Std.List.map ~f:(fun p -> (Printf.sprintf "%s/%s" path p)) paths))
   | _ -> (* Symlinks are not handled as violate the recursive permission model *)
     Lwt.return acc)

let read ~client ~peer ~service ~paths =
  if paths = [] then Lwt.return (`Assoc []) else
    Client.connect client
    >>= fun (c9p,cdk) ->
    (Client.checkout (build_branch ~peer ~service) cdk
     >>= fun branch -> Client.get_head_pointer branch
     >>>= fun ptr -> Lwt.return ptr
     >>= begin function
       | None      -> Lwt.return (`Assoc (Core.Std.List.map paths ~f:(fun file -> (file,`Null))))
       | Some head ->
         (let tree = Client.get_tree head in
          (Lwt_list.fold_left_s (read_path tree) [] paths)
          >|= (fun l -> (`Assoc l)))
     end
     >>= fun r -> (Client.disconnect c9p cdk >|= fun () -> r))

let delete ~client ~peer ~service ~paths =
  if paths = [] then Lwt.return () else
    Client.connect client
    >>= fun (c9p,cdk) ->
    try
      (Client.checkout (build_branch ~peer ~service) cdk
       >>= (fun branch ->
           Log.debug (fun m -> m "Checked out branch %s" service);
           Client.new_transaction branch
           >>>= fun tr -> Log.debug (fun m -> m "Created transaction on branch %s." service);
           (
             let delete_file f =
               try
                 Client.delete_file_or_directory tr (Datakit_path.of_string_exn f)
                 >>>= fun () -> Lwt.return ()
               with
               | Datakit_error "Target does not exist." -> Lwt.return ()
             in
             (try
                Lwt_list.iter_s delete_file paths
                >>= fun () ->
                Log.debug (fun m -> m "Committing transaction.");
                (Client.commit_transaction tr ~message:"Delete paths from silo")
              with
              | _ ->
                Log.info (fun m -> m "Aborting transaction.\n");
                Client.abort_transaction tr >|= fun () -> raise Delete_failed))
           >>>= fun () ->
           (Log.debug (fun m -> m "Disconnecting from %s" (Client.address client));
            Client.disconnect c9p cdk
            >|= fun () -> Log.debug (fun m -> m "Disconnected from %s" (Client.address client)))))
    with _ -> (Log.err (fun m -> m "Aborted transaction.");
               Client.disconnect c9p cdk >|= fun () -> raise (Delete_failed))
