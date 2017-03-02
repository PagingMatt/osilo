(** REST server for osilo *)

class type server = object
  method get_address : Peer.t
  (** [get_addess] returns the [Peer.t] that represents this server. *)

  method get_secret_key : Cstruct.t
  (** [get_secret_key] returns the secret private key for this server. *)

  method get_private_key : Nocrypto.Rsa.priv
  (** [get_private_key] returns the private RSA key for this server. *)

  method get_public_key : Nocrypto.Rsa.pub
  (** [get_public_key] returns the public RSA key for this server. *)

  method get_keying_service : Cryptography.Keying.t
  (** [get_keying_service] returns the public RSA key store for this server. *)

  method set_keying_service : Cryptography.Keying.t -> unit
  (** [get_keying_service ks] sets the public RSA key store for this server to [ks]. *)

  method get_capability_service : Auth.CS.t
  (** [get_capability_service] returns the capability service for this server, this contains the
  capabilities that other peers have given to this peer. *)

  method set_capability_service : Auth.CS.t -> unit
  (** [set_capability_service cs] is a side effecting function to assign the capability service for
  this server to [cs]. *)

  method get_peer_access_log : Peer_access_log.t
  (** [get_peer_access_log] gives the current [Peer_access_log] member of this server. *)

  method set_peer_access_log : Peer_access_log.t -> unit
  (** [set_peer_access_log log] sets the [Peer_access_log] member of this server to [log]. *)

  method get_silo_client : Silo.Client.t
  (** [get_silo_client] returns the [Silo.Client.t] representing the client to this server's
      Datakit instance. *)

  method set_silo_client : Silo.Client.t -> unit

  method get_data_cache : Data_cache.t

  method set_data_cache : Data_cache.t -> unit

  method start : unit Lwt.t
  (** [start] is the non-terminating operation to start this REST server. *)
end

class server' : string -> Cstruct.t -> string -> string -> string -> server
(** Server class encapsulates a [Webmachine] server. *)
