(** Encapsulates a HTTP client suitable for interacting with peer REST servers.
    *)

type auth =
  | None
  | Key of string
  | Sig of string * string
(** Exposed type for passing in authentication credentials for HTTPS headers. *)

val get :
  peer:Peer.t ->
  path:string -> (int * string) Lwt.t
(** [get ~peer ~path] gives a promise to the pair of an [int] HTTP return code
    and [string] response body as the result of performing a HTTP GET to the URI
    comprised of [peer] and [path]. *)

val post :
  peer:Peer.t ->
  path:string ->
  body:string ->
  auth:auth   -> (int *  string) Lwt.t
(** [post ~peer ~path ~body ~key] gives a promise to the pair of an [int] HTTP
    return code and [string] response body as the result of performing a HTTP
    POST of [body] to the URI comprised of [peer] and [path]. [key] is the
    shared secret between the client and [peer] used to authorise the post. *)
