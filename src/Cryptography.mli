(** Provides cryptography for the system *)

open Nocrypto

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
