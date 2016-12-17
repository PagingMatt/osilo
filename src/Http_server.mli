(** REST server for osilo *)

class server : string -> Cstruct.t -> string -> object
  method get_address : Peer.t
  (** [get_addess] returns the [Peer.t] that represents this server. *)

  method get_keying_service : Cryptography.KS.t
  (** [get_keying_service] returns the [Cryptography.KS.t] for this server. *)

  method set_keying_service : Cryptography.KS.t -> unit
  (** [set_keying_service ks] is a side effecting function to assign the keying service for this
  server to [ks]. *)

  method get_secret_key : Cstruct.t
  (** [get_secret_key] returns the secret private key for this server from the keying service. *)

  method get_capability_service : (Auth.Token.t * Auth.M.t) File_tree.t
  (** [get_capability_service] returns the capability service for this server, this contains the 
  capabilities that other peers have given to this peer. *)

  method set_capability_service : (Auth.Token.t * Auth.M.t) File_tree.t -> unit
  (** [set_capability_service cs] is a side effecting function to assign the capability service for 
  this server to [cs]. *)

  method get_silo_client : Silo.Client.t
  (** [get_silo_client] returns the [Silo.Client.t] representing the client to this server's 
  Datakit instance. *)

  method start : unit Lwt.t
  (** [start] is the non-terminating operation to start this REST server. *)
end
(** Server class encapsulates a [Webmachine] server. *)
