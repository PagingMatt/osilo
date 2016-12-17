type t 

val empty : t

val log : t -> host:Peer.t -> peer:Peer.t -> service:string -> path:string -> t
(** [log l ~host ~peer ~service ~path] takes the current peer access log and gives back another where it 
has been recorded that [peer] requested [path] from [host]'s' [service]. *)

val find : t -> host:Peer.t -> service:string -> path:string -> Peer.t list
(** [find l ~host ~service ~path] returns a list of [Peer.t] which have accessed [path] or any file below
[path] in [host]'s [service]. *)