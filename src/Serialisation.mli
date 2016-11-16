(** Provides serialisation and deserialisation of Cstruct.t messages *)

val encode : Cstruct.t -> string 
(** [encode m] takes the [Cstruct.t] [m] and encodes it to a base 64 [string] for transmission *)

exception Decoding_failed
(** Raised if cannot decode [Cstruct.t] *)

val decode : string -> Cstruct.t
(** [decode m] takes the base 64 [string] and attempts to decode it into a [Cstruct.t] *)

(** Internal representation of messages passed around the system *)
module Message : sig
	type t
	(* Internal representation *)

	exception Deserialisation_failed
	(** Thrown when cannot parse deserialised meesage into a Message.t *)

	val serialise : t -> Cstruct.t 
	(** [serialise m] gives a [Cstruct.t] representing [m] *)

	val deserialise : Cstruct.t -> t
	(** [deserialise m] gives a [t] representing [m] *)
end