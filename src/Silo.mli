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

exception Checkout_failed of string * string
exception Connection_failed of string * string
exception Cannot_get_head_commit of string * string
exception Cannot_create_transaction of string * string
exception Cannot_create_parents of string * string
exception Create_or_replace_file_failed of string
exception No_head_commit of string
exception Write_failed of string
exception Delete_failed of string
exception Delete_file_failed of string


val write :
  client:Client.t            ->
  peer:Peer.t                ->
  service:string             ->
  contents:Yojson.Basic.json ->
  unit Lwt.t
(** [write ~client ~peer ~service ~contents] will take the JSON [contents], it expects this to be 
[`Assoc of (string * Yojson.Basic.t) list], the [string]s are file paths and the [Yojson.Basic.json]
is the file contents to write to these file paths. [client] is the client pointing to the correct
Datakit instance, [peer] is the peer that owns the data that is about to be written and the 
[service] is the service this data is for. *)

val read :
  client:Client.t   ->
  peer:Peer.t       ->
  service:string    ->
  paths:string list -> 
  (Yojson.Basic.json) Lwt.t
(** [read ~client ~peer ~service ~paths] will read each file below or at the list of [paths] recursively from the 
[service] on the Datakit server pointed to by [client], for [peer]. [peer] is the [Peer.t] for this
server. *)

val delete :
  client:Client.t   ->
  peer:Peer.t       ->
  service:string    ->
  paths:string list -> 
  unit Lwt.t
(** [delete ~client ~peer ~service ~paths] will delete each file (if it exists) from the list of 
[paths] from the [service] on the Datakit server pointed to by [client], for [peer]. [peer] is the
[Peer.t] for this server. *)
