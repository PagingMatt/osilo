(** Provides serialisation and deserialisation of Cstruct.t messages *)

(** Internal representation of messages passed around the system *)
module Message : sig
	type t
	(* Internal representation *)

	exception Deserialisation_failed
	(** Thrown when cannot parse deserialised meesage into a Message.t *)

	val serialise : Message.t -> Cstruct.t 
	(** [serialise m] gives a [Cstruct.t] representing [m] *)

	val deserialise : Cstruct.t -> Message.t
	(** [deserialise m] gives a [Message.t] representing [m] *)
end