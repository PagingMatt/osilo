type t
(* Internal representation for a peer *)

val host : t -> string
(** Gets host from a peer. TODO consider returning ipaddr or uri *)

val port : t -> int
(** Gets port from peer. TODO consider if int is appropriate - probably is *)

val compare : t -> t-> int
(** Compare two peers - needed for Map.OrderedType satisfaction (keys in cache) *)
