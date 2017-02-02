(** Provides cryptography for the system *)

open Nocrypto

module Serialisation : sig 
  exception Deserialisation_failed of string
  val serialise_cstruct     : Cstruct.t -> string
  val deserialise_cstruct   : string    -> Cstruct.t
  val serialise_encrypted   : ciphertext:Cstruct.t -> iv:Cstruct.t -> string
  val deserialise_encrypted : message:string -> Cstruct.t * Cstruct.t
end

module Signing : sig
  include (module type of Rsa.PSS(Hash.SHA512))
end

val encrypt :
  key:Cstruct.t ->
  plaintext:Cstruct.t -> Cstruct.t * Cstruct.t
(** [encrypt ~key ~plaintext] encrypts the [plaintext] using the secret [key] and returns a pair
containing the ciphertext and the inital vector used. *)

val decrypt :
  key:Cstruct.t ->
  ciphertext:Cstruct.t ->
  iv:Cstruct.t -> Cstruct.t
(** [decrypt ~key ~ciphertext ~iv] decrypts the [ciphertext] with the secret [key], using
the initial vector [iv]. *)
