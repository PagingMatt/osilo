type t

val host : Peer.t -> string

val port : Peer.t -> int

val compare : t -> t-> int