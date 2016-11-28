(** Webmachine REST server *)

class server : string -> int -> Cstruct.t -> string -> object
  method get_address : Peer.t
  method get_keying_service : Cryptography.KS.t
  method set_keying_service : Cryptography.KS.t -> unit
  method start : unit -> unit Lwt.t
end
