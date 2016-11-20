module Client : sig
  type t
  exception Failed_to_make_silo_client of Uri.t
  val make : server:Uri.t -> t
end = struct
  type t = {
    server : string (* Client9p.connect takes a string not a Uri.t *)
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

  module Silo_9p_client = Client9p_unix.Make(
  	val Logs.src (Logs.Src.create "osilo datakit client") : Logs.LOG)
  
  module Silo_datakit_client = Datakit_client_9p.Make(Silo_9p_client)
end
