(** Webmachine REST server *)

class server : string -> int -> Cstruct.t -> string -> object
  method set_keying_service : Cryptography.KS.t -> unit
  method start : unit -> unit Lwt.t
end
