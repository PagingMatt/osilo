open Lwt.Infix
open Result

module Client : sig
  type t
  exception Failed_to_make_silo_client of Uri.t
  val make : server:Uri.t -> t
  val server : t -> string
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
  	val Logs.src (Logs.Src.create "osilo datakit client") : Logs.LOG)
  
  module Silo_datakit_client = Datakit_client_9p.Make(Silo_9p_client)
end

let write ~client ~service ~file ~contents =
	Silo_9p_client.connect (Client.server client) () 
	>>= begin function
	  | Ok conn_9p  -> conn_9p |> Silo_datakit_client.connect
	  | Error error -> raise Write_failed
	  end
	>>= fun conn_dk -> Silo_datakit_client.Branch service 
	>>= begin function
	  | Ok branch   -> branch
	  | Error error -> raise Write_failed
	  end
	>>= fun branch -> 
	  Silo_datakit_client.with_transaction branch 
	  (fun tr -> 
	    let contents' = Cstruct.of_string contents in
	    Silo_datakit_client.Transaction.create_file tr (Datakit_path.of_string_exn file) contents' >>=
	    begin function
        | Ok ()   -> Silo_datakit_client.commit tr ~message:"Write"
        | Error e -> raise Write_failed
	    end)