(** Decoding transmission-suitable (often serialised JSON) [string] types into internal types and
 vice versa. *)

exception Decoding_failed
(** Raised if cannot decode a string. *)

val encode_cstruct : Cstruct.t -> string 
(** [encode_cstruct m] takes the [Cstruct.t] [m] and encodes it to a base 64 [string] for 
transmission. *)

val decode_cstruct : string -> Cstruct.t
(** [decode_cstruct m] takes the base 64 [string] and attempts to decode it into a [Cstruct.t]. *)

val encode_group : Nocrypto.Dh.group -> string
(** [encode_group group] encodes the Diffie-Helmann group in a string. *)

val decode_group : string -> Nocrypto.Dh.group
(** [decode_group message] decodes a Diffie-Helmann group from a string representation. *)

val encode_client_message : ciphertext:Cstruct.t -> iv:Cstruct.t -> string
(** [encode_client_message ~ciphertext ~iv] constructs the JSON string encoding [ciphertext] and
[iv] to send between server and client. *)

val decode_client_message : message:string -> Cstruct.t * Cstruct.t 
(** [decode_peer_message ~message] takes the JSON string [message] which encodes an initial vector and
ciphertext, sent by some client. The result is the pair containing these. *)

val encode_peer_message : peer:Peer.t -> ciphertext:Cstruct.t -> iv:Cstruct.t -> string
(** [encode_peer_message ~peer ~ciphertext ~iv] constructs the JSON string encoding [ciphertext] and
[iv] for this endpoint, [peer]. *)

val decode_peer_message : message:string -> Peer.t * Cstruct.t * Cstruct.t 
(** [decode_peer_message ~message] takes the JSON string [message] which encodes an initial vector and
ciphertext, sent by some [peer]. The result is the tuple containing these. *)

val encode_kx_init : peer:Peer.t -> public:Cstruct.t -> group:Nocrypto.Dh.group -> string
(** [encode_kx_init ~peer ~public ~group] encodes the key exchange initialisation message into a 
JSON string ready for transmission, for this [peer]. *)

val decode_kx_init : message:string -> Peer.t * Cstruct.t * Nocrypto.Dh.group
(** [decode_kx_init ~message] decodes a key exchange initialisation message from the transmitted
JSON string, sent by some remote [peer]. *)

val encode_kx_reply : peer:Peer.t -> public:Cstruct.t -> string
(** [encode_kx_reply ~peer ~public] encodes a key exchange reply into a JSON string from the 
calculated public key, from this [peer]. *)

val decode_kx_reply : message:string -> Peer.t * Cstruct.t
(** [decode_kx_reply ~message] takes a JSON string which represents a key exchange reply message
and decodes it to get the public key and the [peer] that sent it. *)

