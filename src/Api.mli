exception Malformed_data
(** Raised when data passed through a POST is not as expected. *)

exception Path_info_exn of string
(** Raised when expected wildcard doesn't exist in path. *)

type provider_body = Cohttp_lwt_body.t Wm.provider
type acceptor_body = Cohttp_lwt_body.t Wm.acceptor
type 'a content_types = ((string * 'a) list, Cohttp_lwt_body.t) Wm.op

class ping : object
  inherit [Cohttp_lwt_body.t] Wm.resource

  method content_types_provided : provider_body content_types

  method content_types_accepted : acceptor_body content_types
end

module Client : sig 
  class get_local : 
    < get_address : Peer.t; get_secret_key : Cstruct.t;
      get_silo_client : Silo.Client.t; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end

  class get_remote : 
    < get_address : Peer.t; get_capability_service : Auth.CS.t;
      get_keying_service : Cryptography.KS.t; get_secret_key : Cstruct.t;
      get_silo_client : Silo.Client.t;
      set_keying_service : Cryptography.KS.t -> 'a; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end

  class set_local : 
    < get_address : Peer.t; get_secret_key : Cstruct.t;
      get_silo_client : Silo.Client.t; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end

  class set_remote : 
    < get_address : Peer.t; get_capability_service : Auth.CS.t;
      get_keying_service : Cryptography.KS.t; get_secret_key : Cstruct.t;
      set_keying_service : Cryptography.KS.t -> 'a; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end

  class del_local : 
    < get_address : Peer.t; get_secret_key : Cstruct.t;
      get_silo_client : Silo.Client.t; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end

  class del_remote : 
    < get_address : Peer.t; get_capability_service : Auth.CS.t;
      get_keying_service : Cryptography.KS.t; get_secret_key : Cstruct.t;
      set_keying_service : Cryptography.KS.t -> 'a; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end

  class permit : 
    < get_address : Peer.t; get_keying_service : Cryptography.KS.t;
      get_secret_key : Cstruct.t;
      set_keying_service : Cryptography.KS.t -> 'a; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end

  class inv : 
    < get_address : Peer.t; get_keying_service : Cryptography.KS.t;
      get_peer_access_log : Peer_access_log.t; get_secret_key : Cstruct.t;
      set_keying_service : Cryptography.KS.t -> 'a;
      set_peer_access_log : Peer_access_log.t -> unit; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
end

module Peer : sig 
  class get : 
    < get_address : Peer.t; get_keying_service : Cryptography.KS.t;
      get_peer_access_log : Peer_access_log.t; get_secret_key : Cstruct.t;
      get_silo_client : Silo.Client.t;
      set_keying_service : Cryptography.KS.t -> 'a;
      set_peer_access_log : Peer_access_log.t -> 'b; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end

  class set : 
    < get_address : Peer.t; get_keying_service : Cryptography.KS.t;
      get_secret_key : Cstruct.t; get_silo_client : Silo.Client.t;
      set_keying_service : Cryptography.KS.t -> 'a; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end

  class del : 
    < get_address : Peer.t; get_keying_service : Cryptography.KS.t;
      get_secret_key : Cstruct.t; get_silo_client : Silo.Client.t;
      set_keying_service : Cryptography.KS.t -> 'a; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end

  class inv : 
    < get_address : Peer.t; get_keying_service : Cryptography.KS.t;
      get_silo_client : Silo.Client.t;
      set_keying_service : Cryptography.KS.t -> 'a; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end

  class permit : 
    < get_capability_service : Auth.CS.t;
      get_keying_service : Cryptography.KS.t;
      set_capability_service : Auth.CS.t -> 'a;
      set_keying_service : Cryptography.KS.t -> 'b; .. > -> object

    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided : provider_body content_types

    method content_types_accepted : acceptor_body content_types
  end
end

