(** Webmachine REST server *)

val start : port:int -> unit Lwt.t
(** [start ~port] starts a Webmachine REST server on [port] *)