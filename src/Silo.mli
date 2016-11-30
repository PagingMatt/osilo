(** Interface to Datakit server pointing to my silo *)

module Client : sig
  type t
  exception Failed_to_make_silo_client of Uri.t
  (** [Failed_to_make_silo_client s] is thrown when the [Uri.t] [s] has either no port or no host 
  meaning that a [t] cannot be built of it *)
  val create : server:string -> t
  (** [make ~server] gives a [t] for [server]*)
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
  (** A 9P UNIX client made by applying a source log to the functor *)
  module Silo_datakit_client : sig
    include Datakit_S.CLIENT with type error = Protocol_9p_error.error
    val connect : Silo_9p_client.t -> t
  end
  (** Datakit client made by applying [Silo_9p_client] to the functor *)
end
(** [Client] module abstracts some client-specific behaviour *)

exception Checkout_failed
exception Write_failed
exception Read_failed

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
  client:Client.t   ->
  service:string    ->
  files:string list -> 
  (Yojson.Basic.json) Lwt.t
(** [read ~service ~files] will return the contents of each file in [files] on the [service] branch 
in my silo repository. *)
