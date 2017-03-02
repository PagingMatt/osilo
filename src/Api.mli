(** Encapsulates all instantiations of Webmachine classes for REST server. *)

exception Malformed_data
(** Raised when data passed through a POST is not as expected. *)

exception Path_info_exn of string
(** Raised when expected wildcard doesn't exist in path. *)

type provider_body = Cohttp_lwt_body.t Wm.provider
(** Reduce replication with [provider_body] type. *)

type acceptor_body = Cohttp_lwt_body.t Wm.acceptor
(** Reduce replication with [acceptor_body] type. *)

type 'a content_types = ((string * 'a) list, Cohttp_lwt_body.t) Wm.op
(** Parameterise [content_types] by either [provider_body] or [acceptor_body]. *)

class ping : object
  inherit [Cohttp_lwt_body.t] Wm.resource

  method content_types_provided : provider_body content_types

  method content_types_accepted : acceptor_body content_types
end
(** Entrypoint for pinging a peer to check liveness. *)

val sign : string -> <get_private_key : Nocrypto.Rsa.priv; ..> -> string
(** Function to sign a message. *)

module Client : sig
  class get_local :
    < get_data_cache : Data_cache.t; set_data_cache : Data_cache.t -> unit;
      get_address : Peer.t; get_secret_key : Cstruct.t; set_silo_client: Silo.Client.t -> unit;
      get_silo_client : Silo.Client.t; get_private_key : Nocrypto.Rsa.priv;
      get_public_key : Nocrypto.Rsa.pub; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for client to read its own data. *)

  class get_remote :
    < get_address : Peer.t; get_capability_service : Auth.CS.t; set_silo_client: Silo.Client.t -> unit;
      get_secret_key : Cstruct.t; get_private_key : Nocrypto.Rsa.priv;
      get_public_key : Nocrypto.Rsa.pub; get_data_cache : Data_cache.t; set_data_cache : Data_cache.t -> unit;
      get_silo_client : Silo.Client.t; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for client to get peer to read another peer's data. *)

  class set_local :
    < get_address : Peer.t; get_secret_key : Cstruct.t; set_silo_client: Silo.Client.t -> unit;
      get_private_key : Nocrypto.Rsa.priv; get_public_key : Nocrypto.Rsa.pub;
      get_silo_client : Silo.Client.t; get_data_cache : Data_cache.t; set_data_cache : Data_cache.t -> unit; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for client to write its own data. *)

  class set_remote :
    < get_data_cache : Data_cache.t; set_data_cache : Data_cache.t -> unit;
      get_address : Peer.t; get_capability_service : Auth.CS.t; set_silo_client: Silo.Client.t -> unit;
      get_secret_key : Cstruct.t; get_silo_client : Silo.Client.t; get_public_key : Nocrypto.Rsa.pub;
      get_private_key : Nocrypto.Rsa.priv; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for client to get peer to write another peer's data. *)

  class del_local :
    < get_address : Peer.t; get_secret_key : Cstruct.t; set_silo_client: Silo.Client.t -> unit;
      get_private_key : Nocrypto.Rsa.priv; get_public_key : Nocrypto.Rsa.pub; get_data_cache : Data_cache.t; set_data_cache : Data_cache.t -> unit;
      get_silo_client : Silo.Client.t; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for client to delete its own data. *)

  class del_remote :
    < get_address : Peer.t; get_capability_service : Auth.CS.t;
      get_public_key : Nocrypto.Rsa.pub; get_data_cache : Data_cache.t; set_data_cache : Data_cache.t -> unit;
      get_silo_client : Silo.Client.t; get_secret_key : Cstruct.t;
      get_private_key : Nocrypto.Rsa.priv; set_silo_client: Silo.Client.t -> unit; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for client to get peer to delete another peer's data. *)

  class permit :
    < get_address : Peer.t; set_silo_client: Silo.Client.t -> unit; get_public_key : Nocrypto.Rsa.pub; get_data_cache : Data_cache.t; set_data_cache : Data_cache.t -> unit;
      get_secret_key : Cstruct.t; get_private_key : Nocrypto.Rsa.priv; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for client to get its peer to mint and send capabilities to another peer. *)

  class inv :
    < get_address : Peer.t; get_public_key : Nocrypto.Rsa.pub; get_data_cache : Data_cache.t; set_data_cache : Data_cache.t -> unit;
      get_peer_access_log : Peer_access_log.t; get_secret_key : Cstruct.t;
      set_peer_access_log : Peer_access_log.t -> unit; set_silo_client: Silo.Client.t -> unit;
      get_private_key : Nocrypto.Rsa.priv; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for client to get its peer to remotely invalidate its data remotely cached. *)
end
(** Entrypoint for all C2P communication. All data posted here is portable/cross-platform.
Clients can be implemented in any language with HTTPS posting and a JSON parser. *)

module Peer : sig
  class pub : <get_public_key : Nocrypto.Rsa.pub; ..> -> object
    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for peer to ask for another peer's public RSA key. *)

  class get :
    < get_address : Peer.t; get_keying_service : Cryptography.Keying.t;
      set_keying_service : Cryptography.Keying.t -> unit; get_data_cache : Data_cache.t; set_data_cache : Data_cache.t -> unit;
      get_peer_access_log : Peer_access_log.t; get_secret_key : Cstruct.t;
      get_silo_client : Silo.Client.t; set_silo_client: Silo.Client.t -> unit;
      set_peer_access_log : Peer_access_log.t -> 'b; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for a peer to read this peer's data. *)

  class set :
    < get_address : Peer.t; get_keying_service : Cryptography.Keying.t; get_data_cache : Data_cache.t; set_data_cache : Data_cache.t -> unit;
      set_keying_service : Cryptography.Keying.t -> unit; set_silo_client: Silo.Client.t -> unit;
      get_secret_key : Cstruct.t; get_silo_client : Silo.Client.t; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for a peer wanting to write this peer's data. *)

  class del :
    < get_address : Peer.t; get_keying_service : Cryptography.Keying.t; get_data_cache : Data_cache.t; set_data_cache : Data_cache.t -> unit;
      set_keying_service : Cryptography.Keying.t -> unit; set_silo_client: Silo.Client.t -> unit;
      get_secret_key : Cstruct.t; get_silo_client : Silo.Client.t; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for a peer wanting to delete this peer's data. *)

  class inv :
    < get_address : Peer.t; get_keying_service : Cryptography.Keying.t;
      set_keying_service : Cryptography.Keying.t -> unit; get_data_cache : Data_cache.t; set_data_cache : Data_cache.t -> unit;
      get_silo_client : Silo.Client.t; set_silo_client: Silo.Client.t -> unit; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for a peer wanting to invalidate its data cached at this peer. *)

  class permit :
    < get_capability_service : Auth.CS.t; get_keying_service : Cryptography.Keying.t; get_data_cache : Data_cache.t; set_data_cache : Data_cache.t -> unit;
      set_keying_service : Cryptography.Keying.t -> unit; set_silo_client: Silo.Client.t -> unit;
      set_capability_service : Auth.CS.t -> 'a; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
  (** Entrypoint for a peer wanting to give a capability to this peer. *)
end
(** Entrypoint for all P2P communication. All data posted here is completely dependent on
the osilo platform. *)
