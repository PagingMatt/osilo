(** Provides a module to abstract peers in the platform. *)

type t
(* Internal representation for a peer. *)

val create : string -> t
(** [create host] builds a [t] from a string [host]. *)

val host : t -> string
(** [host peer] gives the [string] representing the host name of [peer]. *)

val compare : t -> t -> int
(** This makes a uri from the peer and then uses the [Uri] module's compare
    function. *)
