(** Decoding transmission-suitable (often serialised JSON) [string] types into internal types and
 vice versa. *)

type requested_file = {
  path        : string ;
  check_cache : bool   ;
  write_back  : bool   ;
}
(** Internal type for requested files and caching data about these *)

val encode_json_requested_file : requested_file -> Yojson.Basic.json

val decode_json_requested_file : Yojson.Basic.json -> requested_file

exception Decoding_failed of string
(** Raised if cannot decode a string. *)

val encode_cstruct : Cstruct.t -> string 
(** [encode_cstruct m] takes the [Cstruct.t] [m] and encodes it to a base 64 [string] for 
transmission. *)

val decode_cstruct : string -> Cstruct.t
(** [decode_cstruct m] takes the base 64 [string] and attempts to decode it into a [Cstruct.t]. *)

val encode_client_message : ciphertext:Cstruct.t -> iv:Cstruct.t -> string
(** [encode_client_message ~ciphertext ~iv] constructs the JSON string encoding [ciphertext] and
[iv] to send between server and client. *)

val decode_client_message : message:string -> Cstruct.t * Cstruct.t 
(** [decode_peer_message ~message] takes the JSON string [message] which encodes an initial vector and
ciphertext, sent by some client. The result is the pair containing these. *)

val encode_public_key : Nocrypto.Rsa.pub -> string
(** [encode_public_key pub] encodes the RSA public key [pub] in a JSON string. *)

val decode_public_key : string -> Nocrypto.Rsa.pub
(** [decode_public_key msg] decodes the JSON string [msg] into the corresponding RSA public key. *)

val encode_peer_message : peer:Peer.t -> ciphertext:Cstruct.t -> string
(** [encode_peer_message ~peer ~ciphertext] constructs the JSON string encoding [ciphertext] for this 
endpoint, [peer]. *)

val decode_peer_message : message:string -> Peer.t * Cstruct.t 
(** [decode_peer_message ~message] takes the JSON string [message] which encodes a
ciphertext, sent by some [peer]. The result is the tuple containing these. *)

