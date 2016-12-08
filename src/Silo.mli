(** Interface to Datakit server pointing to my silo *)

module Log : sig
  include Logs.LOG
end

module Client : sig
  type t
  exception Failed_to_make_silo_client of Uri.t
  (** [Failed_to_make_silo_client s] is thrown when the [Uri.t] [s] has either no port or no host 
  meaning that a [t] cannot be built of it *)
  val create : server:string -> t
  (** [make ~server] gives a [t] for [server]*)
  module Silo_9p_client : sig
    include (module type of Client9p_unix.Make(Log))
  end
  (** A 9P UNIX client made by applying a source log to the functor *)
  module Silo_datakit_client : sig
    include (module type of Datakit_client_9p.Make(Silo_9p_client))
  end
  (** Datakit client made by applying [Silo_9p_client] to the functor *)
end
(** [Client] module abstracts some client-specific behaviour *)

exception Checkout_failed
exception Write_failed
exception Read_failed

val read :
  client:Client.t   ->
  peer:Peer.t       ->
  service:string    ->
  files:string list -> 
  (Yojson.Basic.json) Lwt.t
(** [read ~client ~peer ~service ~files] will read each file from the list of [files] from the 
[service] on the Datakit server pointed to by [client], for [peer]. [peer] is the [Peer.t] for this
server. *)
