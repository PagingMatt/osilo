(** Provides a module to abstract peers in the platform. *)

type t
(* Internal representation for a peer. *)

val create : string -> int -> t
(** [create host port] builds a [t] from a string [host] and int [port]. *)

val host : t -> string
(** [host peer] gives the [string] representing the host name of [peer]. *)

val port : t -> int
(** [port peer] gives the [int] representing the port number of [peer]. *)

val compare : t -> t -> int
(** This makes a uri from the peer and then uses the [Uri] module's compare
    function. *)
