val get : peer:Peer.t -> path:string -> (int * string) Lwt.t
(** Carries out a HTTP GET request to the URI made from the peer and path provided *)

val post : peer:Peer.t -> path:string -> body:string -> (int *  string) Lwt.t 
(** Does a HTTP POST of the given body to the given peer on the given path *)
