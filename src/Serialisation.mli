(** Provides serialisation and deserialisation of Cstruct.t messages *)

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