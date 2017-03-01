open Lwt.Infix
open Protocol_9p

let src = Logs.Src.create ~doc:"logger for datakit entrypoint" "osilo.silo"
module Log = (val Logs.src_log src : Logs.LOG)

module Client : sig
  type t
  exception Failed_to_make_silo_client of Uri.t
  val create : server:string -> t
  val server : t -> string
  val lookup : path:string -> client:t -> (Yojson.Basic.json * t) option
  val set    : path:string -> file:Yojson.Basic.json -> client:t -> t
  val remove : path:string -> client:t -> t
  module Silo_9p_client : sig
    include (module type of Client9p_unix.Make(Log))
  end
  module Silo_datakit_client : sig
    include (module type of Datakit_client_9p.Make(Silo_9p_client))
  end
end = struct
  module V = struct
    type t = Yojson.Basic.json
    let weight j = Yojson.Basic.to_string j |> String.length
  end

  module L1_C = Lru.F.Make(Core.Std.String)(V)

  type t = {
    server : string ;
    cache  : L1_C.t ;
  }

  let lookup ~path ~client =
    match L1_C.find path client.cache with
    | None       -> None
    | Some (r,c) -> Some (r,{client with cache=c})

  let set ~path ~file ~client =
    let c = L1_C.add path file client.cache in
    {client with cache = c}

  let remove ~path ~client =
    let c = L1_C.remove path client.cache in
    {client with cache = c}

  exception Failed_to_make_silo_client of Uri.t

  let create ~server =
    let address = Printf.sprintf "%s:5640" server in
    { server = address ; cache = L1_C.empty 1000}

  let server c = c.server

  module Silo_9p_client = Client9p_unix.Make(Log)

  module Silo_datakit_client = Datakit_client_9p.Make(Silo_9p_client)
end

exception Connection_failed of string * string

exception Datakit_error of string

exception Write_failed of string
exception Delete_failed

open Client.Silo_datakit_client

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
  >>>= fun branch -> Lwt.return branch

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
    >>>= fun () -> Lwt.return path

let build_branch ~peer ~service =
  Printf.sprintf "%s/%s" (Peer.host peer) service

let write ~client ~peer ~service ~contents =
  let content =
    match contents with
    | `Assoc l -> l
    | _        -> raise (Write_failed "Invalid file content.") in
  if content = [] then Lwt.return () else
    connect client
    >>= fun (c9p,cdk) ->
    Log.debug (fun m -> m "Connected to Datakit server.");
    try
      (checkout (build_branch ~peer ~service) cdk
       >>= (fun branch ->
           Log.debug (fun m -> m "Checked out branch %s" service);
           Branch.transaction branch
           >>>= fun tr -> Log.debug (fun m -> m "Created transaction on branch %s." service);
           (let write_file (f,c) =
              let c' = Yojson.Basic.to_string c |> Cstruct.of_string in
              walk_path_exn f tr
              >>= (fun p -> Transaction.create_or_replace_file tr p c')
              >>>= Lwt.return
            in
            (try
               Lwt_list.iter_s write_file content
               >>= fun () ->
               Log.debug (fun m -> m "Committing transaction.");
               (Transaction.commit tr ~message:"Write to silo")
             with
             | _ ->
               Log.info (fun m -> m "Aborting transaction.");
               Transaction.abort tr >|= fun () -> Ok ()))
           >>>= fun () ->
           (Log.debug (fun m -> m "Disconnecting from %s" (Client.server client));
            disconnect c9p cdk
            >|= fun () -> Log.debug (fun m -> m "Disconnected from %s" (Client.server client)))))
    with _ -> disconnect c9p cdk

let rec read_path tree acc path =
  Client.Silo_datakit_client.Tree.read tree (Datakit_path.of_string_exn path)
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
  let hit,miss,client' = Core.Std.List.fold ~init:([],[],client)
    ~f:(fun (h,m,c) -> fun p ->
         match Client.lookup (Printf.sprintf "%s/%s/%s" (Peer.host peer) service p) c with
         | None         -> h,p::m,c
         | Some (j, c') -> (p,j)::h,m,c') paths in
  if miss = [] then Lwt.return (`Assoc []) else
    connect client'
    >>= fun (c9p,cdk) ->
    (checkout (build_branch ~peer ~service) cdk
     >>= fun branch -> Client.Silo_datakit_client.Branch.head branch
     >>>= fun ptr -> Lwt.return ptr
     >>= begin function
       | None      -> Lwt.return (Core.Std.List.map paths ~f:(fun file -> (file,`Null)))
       | Some head ->
         (let tree = Client.Silo_datakit_client.Commit.tree head in
          Lwt_list.fold_left_s (read_path tree) [] paths)
     end
     >>= fun r -> (disconnect c9p cdk >|= fun () ->
                   (let client'' = Core.Std.List.fold ~init:client'
                        ~f:(fun c -> fun (p,j) -> Client.set p j c) r in `Assoc (r @ hit))))

let delete ~client ~peer ~service ~paths =
  if paths = [] then Lwt.return () else
    connect client
    >>= fun (c9p,cdk) ->
    try
      (checkout (build_branch ~peer ~service) cdk
       >>= (fun branch ->
           Log.debug (fun m -> m "Checked out branch %s" service);
           Branch.transaction branch
           >>>= fun tr -> Log.debug (fun m -> m "Created transaction on branch %s." service);
           (
             let delete_file f =
               try
                 Transaction.remove tr (Datakit_path.of_string_exn f)
                 >>>= fun () -> Lwt.return ()
               with
               | Datakit_error "Target does not exist." -> Lwt.return ()
             in
             (try
                Lwt_list.iter_s delete_file paths
                >>= fun () ->
                Log.debug (fun m -> m "Committing transaction.");
                (Transaction.commit tr ~message:"Delete paths from silo")
              with
              | _ ->
                Log.info (fun m -> m "Aborting transaction.\n");
                Transaction.abort tr >|= fun () -> raise Delete_failed))
           >>>= fun () ->
           (Log.debug (fun m -> m "Disconnecting from %s" (Client.server client));
            disconnect c9p cdk
            >|= fun () ->
            let client' = Core.Std.List.fold ~init:client ~f:(fun c -> fun p ->
                let path = Printf.sprintf "%s/%s/%s" (Peer.host peer) service p in
                Client.remove ~path ~client:c) paths in
            Log.debug (fun m -> m "Disconnected from %s" (Client.server client)))))
    with _ -> (Log.err (fun m -> m "Aborted transaction.");
               disconnect c9p cdk >|= fun () -> raise (Delete_failed))
