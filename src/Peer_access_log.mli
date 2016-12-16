type t 

val create : t

val log : t -> peer:Peer.t -> service:string -> path:string -> t
(** [log l ~peer ~service ~path] takes the current peer access log and gives back another where it 
has been recorded that [peer] requested [path] from my [service]. *)

val find : t -> service:string -> path:string -> Peer.t list
(** [find l ~service ~path] returns a list of [Peer.t] which have accessed [path] or any file below
[path] in my [service]. *)