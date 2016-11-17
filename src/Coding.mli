(** Bridging between [string] and [Cstruct.t] messages *)

val encode : Cstruct.t -> string 
(** [encode m] takes the [Cstruct.t] [m] and encodes it to a base 64 [string] for transmission *)

exception Decoding_failed
(** Raised if cannot decode [Cstruct.t] *)

val decode : string -> Cstruct.t
(** [decode m] takes the base 64 [string] and attempts to decode it into a [Cstruct.t] *)