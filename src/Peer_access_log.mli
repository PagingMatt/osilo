(** Encapsulates the log of other peers reading this peer's data. *)

type t
(** Internal type for peer access log. *)

val empty : t
(** [empty] value gives an empty peer access log. *)

val log :
  t              ->
  host:Peer.t    ->
  peer:Peer.t    ->
  service:string ->
  path:string    -> t
(** [log l ~host ~peer ~service ~path] takes the current peer access log and
    gives back another where it has been recorded that [peer] requested [path]
    from [host]'s' [service]. *)

val delog :
  t              -> 
  host:Peer.t    ->
  service:string ->
  path:string    -> (Peer.t list * t)
(** [unlog l ~host ~service ~path] is used to remove all log entries at and
    below [host]/[service]/[path] in the log [l], returned in a pair with the
    flattened peers at and below [host]/[service]/[path].*)
