(** Interface to Datakit server pointing to my silo *)

module Client : sig
  type t
  exception Failed_to_make_silo_client of Uri.t
  (** [Failed_to_make_silo_client s] is thrown when the [Uri.t] [s] has either no port or no host 
  meaning that a [t] cannot be built of it *)
  val make : server:Uri.t -> t
  (** [make ~server] gives a [t] for [server]*)
end
(** [Client] module abstracts some client-specific behaviour *)

exception Checkout_failed
exception Write_failed

val write :
  client:Client.t ->
  service:string -> 
  file:string ->
  contents:Yojson.Basic.json -> 
  unit Lwt.t
(** [write ~service ~file ~contents] writes the value of [contents] to [file] on the [service]
branch in my silo repository. This will overwrite anything currently in this file without warning. 
*)

val read :
  client:Client.t ->
  service:string ->
  file:string -> 
  (Yojson.Basic.json option) Lwt.t
(** [read ~service ~file] will return the contents of the [file] on the [service] branch in my silo
repository. This is an option type so if this doesn't exist then [None] is returned. *)