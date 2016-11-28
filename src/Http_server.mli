(** Webmachine REST server *)

class server : string -> int -> Cstruct.t -> string -> object
  method start : unit -> unit Lwt.t
end
