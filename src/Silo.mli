(** Encapsulates the interface to Datakit server pointing to data silo. *)

module Log : sig
  include Logs.LOG
end
(** Needed for instantiating a [Client9p_unix] *)

module Client : sig
  type t
  (** Internal type for a client. *)

  exception Failed_to_make_silo_client of Uri.t
  (** [Failed_to_make_silo_client s] is thrown when the [Uri.t] [s] has either
      no port or no host meaning that a [t] cannot be built of it. *)

  val create : server:string -> t
  (** [make ~server] gives a [t] for [server]. *)
end
(** [Client] module abstracts some client-specific behaviour. It encapsulates
    the two clients needed to interact with Datakit and operations on these,
    however externally the only exposed function is to create a client. *)

exception Connection_failed of string * string
(** Raised when connecting to the Datakit server fails. *)

exception Datakit_error of string
(** Raised when an operation inside Datakit fails. This will specify the exact
    Datakit error. *)

exception Write_failed of string
(** Raised when writing fails. As [write] gives [unit], without an exception
    there would be no feedback if writing failed. *)

exception Delete_failed
(** Raised when writing fails. As [delete] gives [unit], without an exception
    there would be no feedback if deleting failed. *)

val write :
  client:Client.t            ->
  peer:Peer.t                ->
  service:string             ->
  contents:Yojson.Basic.json -> unit Lwt.t
(** [write ~client ~peer ~service ~contents] will take the JSON [contents], it
    expects this to be [`Assoc of (string * Yojson.Basic.t) list], the [string]s
    are file paths and the [Yojson.Basic.json] is the file contents to write to
    these file paths. [client] is the client pointing to the correct Datakit
    instance, [peer] is the peer that owns the data that is about to be written
    and the [service] is the service this data is for. *)

val read :
  client:Client.t   ->
  peer:Peer.t       ->
  service:string    ->
  paths:string list -> (Yojson.Basic.json) Lwt.t
(** [read ~client ~peer ~service ~paths] will read each file below or at the
    list of [paths] recursively from the [service] on the Datakit server pointed
    to by [client], for [peer]. [peer] is the [Peer.t] for this server. *)

val delete :
  client:Client.t   ->
  peer:Peer.t       ->
  service:string    ->
  paths:string list ->
  unit Lwt.t
(** [delete ~client ~peer ~service ~paths] will delete each file (if it exists)
    from the list of [paths] from the [service] on the Datakit server pointed to
    by [client], for [peer]. [peer] is the [Peer.t] for this server. *)
