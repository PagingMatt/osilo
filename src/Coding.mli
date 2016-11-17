(** Bridging between [string] and [Cstruct.t] messages. *)

exception Decoding_failed
(** Raised if cannot decode [Cstruct.t]. *)

val encode_cstruct : Cstruct.t -> string 
(** [encode_cstruct m] takes the [Cstruct.t] [m] and encodes it to a base 64 [string] for 
transmission. *)

val decode_cstruct : string -> Cstruct.t
(** [decode_cstruct m] takes the base 64 [string] and attempts to decode it into a [Cstruct.t]. *)

val encode_group : Nocrypto.Dh.group -> string
(** [encode_group group] encodes the Diffie-Helmann group in a string. *)

val decode_group : string -> Nocrypto.Dh.group
(** [decode_group message] decodes a Diffie-Helmann group from a string representation. *)

val encode_message : ciphertext:Cstruct.t -> iv:Cstruct.t -> string
(** [encode_encrypted ~ciphertext ~iv] constructs the JSON string encoding [ciphertext] and [iv]. *)

val decode_message : message:string -> Cstruct.t * Cstruct.t 
(** [decode_encrypted ~message] takes the JSON string [message] which encodes an initial vector and
ciphertext. The result is the pair containing these. *)

val encode_kx_init : public:Cstruct.t -> group:Nocrypto.Dh.group -> string
(** [encode_kx_init ~public ~group] encodes the key exchange initialisation message into a JSON 
string ready for transmission. *)

val decode_kx_init : message:string -> Cstruct.t * Nocrypto.Dh.group
(** [decode_kx_init ~message] decodes a key exchange initialisation message from the transmitted
JSON string. *)

val encode_kx_reply : public:Cstruct.t -> string
(** [encode_kx_reply ~public] encodes a key exchange reply into a JSON string from the calculated
public key. *)

val decode_kx_reply : message:string -> Cstruct.t
(** [decode_kx_reply ~message] takes a JSON string which represents a key exchange reply message
and decodes it to get the public key. *)