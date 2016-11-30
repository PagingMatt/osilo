(** Webmachine REST server *)

class server : string -> int -> Cstruct.t -> string -> object
  method get_address : Peer.t
  method get_keying_service : Cryptography.KS.t
  method set_keying_service : Cryptography.KS.t -> unit
  method get_secret_key : Cstruct.t
  method get_silo_client : Silo.Client.t
  method start : unit Lwt.t
end
