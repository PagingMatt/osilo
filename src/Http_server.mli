(** Webmachine REST server *)

val start : port:int -> silo:Uri.t -> master:Cstruct.t -> unit Lwt.t
(** [start ~port] starts a Webmachine REST server on [port] *)
