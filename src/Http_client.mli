(** Abstract HTTP client *)

val get : 
  peer:Peer.t -> 
  path:string -> (int * string) Lwt.t
(** [get ~peer ~path] gives a promise to the pair of an integer HTTP return code and string 
response body as the result of performing a HTTP GET to the URI comprised of [peer] and [path]. *)

val post : 
  peer:Peer.t -> 
  path:string -> 
  body:string -> (int *  string) Lwt.t 
(** [post ~peer ~path ~body] gives a promise to the pair of an integer HTTP return code and string 
response body as the result of performing a HTTP POST of [body] to the URI comprised of [peer] and
[path]. *)
