(** Provides a module to abstract the view endpoints/peers in the system *)

type t
(* Internal representation for a peer *)

val create : string -> t
(** [create host] builds a [t] from a string host *)

val host : t -> string
(** [host peer] gives the string representing the host name of [peer] *)

val port : t -> int
(** [port peer] gives the integer representing the port number of [peer], this 
is fixed at 6620. *)

val compare : t -> t-> int
(** [compare p1 p2] is necessary for Peer.t to be used as a key in the [Lru_map.F] cache 
implementation such that Peer.t satisfies the Map.OrderedType signature. This makes a uri from the
peer and then uses the [Uri] module's compare function. *)
