open Lwt.Infix
open Result
open Protocol_9p

module Client : sig
  type t
  exception Failed_to_make_silo_client of Uri.t
  val make : server:Uri.t -> t
  val server : t -> string
  module Silo_9p_client : sig
    include Protocol_9p.Client.S
    val connect:
      string -> 
      string -> 
      ?msize:int32 -> 
      ?username:string -> 
      ?aname:string ->
      unit -> 
      t Protocol_9p.Error.t Lwt.t
  end
  module Silo_datakit_client : sig
    include Datakit_S.CLIENT with type error = Protocol_9p_error.error
    val connect : Silo_9p_client.t -> t
  end
end = struct
  type t = {
    server : string (* Silo_9p_client.connect takes a string not a Uri.t *)
  }

  exception Failed_to_make_silo_client of Uri.t

  let make ~server =
    let h = 
      (match Uri.host server with 
      | Some h' -> h'
      | None    -> raise (Failed_to_make_silo_client server))
    in
    let p = 
      (match Uri.port server with
      | Some p' -> p'
      | None    -> raise (Failed_to_make_silo_client server))
    in
    let s = (Printf.sprintf "%s:%d" h p) in
    { server = s }

  let server c = c.server

  module Silo_9p_client = Client9p_unix.Make(
    (val Logs.src_log (Logs.Src.create "osilo datakit client") : Logs.LOG) )
  
  module Silo_datakit_client = Datakit_client_9p.Make(Silo_9p_client)
end
  
exception Checkout_failed
exception Write_failed

let checkout client service = 
  Client.Silo_9p_client.connect "tcp" (Client.server client) () 
  >|= begin function
      | Ok conn_9p  -> conn_9p |> Client.Silo_datakit_client.connect
      | Error error -> raise Checkout_failed
      end
  >>= fun conn_dk -> Client.Silo_datakit_client.branch conn_dk service 
  >|= begin function
      | Ok branch   -> branch
      | Error error -> raise Checkout_failed
      end

let write ~client ~service ~file ~contents =
  checkout client service
  >>= fun branch -> 
    Client.Silo_datakit_client.Branch.with_transaction branch 
      (fun tr -> 
      let contents' = Yojson.Basic.to_string contents |> Cstruct.of_string in
      Client.Silo_datakit_client.Transaction.create_or_replace_file tr (Datakit_path.of_string_exn file) contents' >>=
      begin function
      | Ok ()   -> Client.Silo_datakit_client.Transaction.commit tr "Write"
      | Error e -> raise Write_failed
      end)
  >|= begin function
      | Ok () -> ()
      | Error e -> raise Write_failed
      end

let read ~client ~service ~file =
  checkout client service
  >>= fun branch -> 
    Client.Silo_datakit_client.Branch.with_transaction branch 
    (fun tr -> Client.Silo_datakit_client.Transaction.read_file tr (Datakit_path.of_string_exn file))
  >|= begin function
      | Ok cstruct  -> Some (cstruct |> Cstruct.to_string |> Yojson.Basic.from_string)
      | Error error -> None
      end
