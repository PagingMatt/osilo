(** Provides cryptography for the system *)

open Nocrypto

module Serialisation : sig
  exception Deserialisation_failed of string
  (** [Deserialisation_failed msg] is raised when the input to a deserialisation function
  is invalid. *)

  val serialise_cstruct : Cstruct.t -> string
  (** [serialise_cstruct cstruct] gives back the [string] representation of [cstruct]. *)

  val deserialise_cstruct : string -> Cstruct.t
  (** [deserialise_cstruct msg] attempts to deserialise [msg] into a [Cstruct.t], raising
  [Deserialisation_failed msg] if [msg] cannot be deserialised into a [Cstruct.t]. *)

  val serialise_encrypted : ciphertext:Cstruct.t -> iv:Cstruct.t -> string
  (** [serialise_encrypted ~ciphertext ~iv] gives a string which represents [ciphertext]
  and [iv]. *)

  val deserialise_encrypted : message:string -> Cstruct.t * Cstruct.t
  (** [deserialise_encrypted ~message] takes the [string] [message] and attempts to
  deserialise this back into the ciphertext, initial vector pair that [message]
  represents. Failing this it raises [Deserialisation_failed mesage]. *)
end
(** Encapsulates serialisation functions for the cryptographic types. *)

module Signing : sig
  include (module type of Rsa.PSS(Hash.SHA512))
end
(** Instantiation of PSS RSA signing functor with SHA512 hash. *)

module Keying : sig
  type t
  (** Abstracted type of the keying module which represents the key store. *)

  exception Public_key_not_found of Peer.t
  (** When a key store doesn't contain the public key of [peer] and looking it up
  from the REST API for [peer] fails, [Public_key_not_found peer] is raised. *)

  val empty : capacity:int -> t
  (** [empty ~capacity] creates an empty key store of size [capacity]. *)

  val invalidate : ks:t -> peer:Peer.t -> t
  (** [invalidate ~ks ~peer] returns [ks] where if [peer] is in [ks], it is
  removed and otherwise [ks] is unchanged. *)

  val lookup : ks:t -> peer:Peer.t -> (t * Nocrypto.Rsa.pub) Lwt.t
  (** [lookup ~ks ~peer] gives the promise of the public key of [peer] along
  with [ks] updated so that [peer] is both in it and having been recorded so
  the LRU statistics are updated. If [peer] was not in [ks] when this was
  called, its public key is found from its REST API and stored in [ks]. If
  [peer] is not in [ks] and its public key cannot be retrieved from its REST
  API, then [Public_key_not_found peer] is raised. *)
end
(** Encapsulation of RSA public key store with transparent lookup *)


val encrypt :
  key:Cstruct.t ->
  iv:Cstruct.t  ->
  plaintext:Cstruct.t -> Cstruct.t
(** [encrypt ~key ~iv ~plaintext] encrypts the [plaintext] using the secret
    [key] and initial vector [iv], then returns the ciphertext. *)

val decrypt :
  key:Cstruct.t ->
  ciphertext:Cstruct.t ->
  iv:Cstruct.t -> Cstruct.t
(** [decrypt ~key ~ciphertext ~iv] decrypts the [ciphertext] with the secret
    [key], using the initial vector [iv]. *)
