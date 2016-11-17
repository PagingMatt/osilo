(** Bridging between [string] and [Cstruct.t] messages *)

exception Decoding_failed
(** Raised if cannot decode [Cstruct.t] *)

val encode_cstruct : Cstruct.t -> string 
(** [encode_cstruct m] takes the [Cstruct.t] [m] and encodes it to a base 64 [string] for transmission *)

val decode_cstruct : string -> Cstruct.t
(** [decode_cstruct m] takes the base 64 [string] and attempts to decode it into a [Cstruct.t] *)

val encode_message : ciphertext:Cstruct.t -> iv:Cstruct.t -> string
(** [encode_encrypted ~ciphertext ~iv] constructs the JSON string encoding [ciphertext] and [iv] *)

val decode_message : message:string -> Cstruct.t * Cstruct.t 
(** [decode_encrypted ~message] takes the JSON string [message] which encodes an initial vector and
ciphertext. The result is the pair containing these. *)