(** Provides keying and cryptography for the system *)

open Nocrypto
open Lwt
open Lwt.Infix

exception Key_exchange_failed
(** Generic exception thrown when key exchange fails anywhere. *)

(** Keying service carries out operations on a functional key cache *)
module KS : sig
  type t
  (* Internal representation fo the keying service *)

  val empty : capacity:int -> t
  (** Constructor for keying service with an empty cache. [empty ~capacity] gives a keying service
  with an empty cache that can hold [capacity] keys. *)

  val invalidate : ks:t -> peer:Peer.t -> t 
  (** [invalidate ~ks ~peer] gives the keying service equal to [ks] without a mapping from [peer]
  in its cache. If [peer] wasn't in [ks]'s cache this is just [ks]. *)

  val mediate : ks:t -> peer:Peer.t -> group:Dh.group -> public:Cstruct.t -> t * Cstruct.t
  (** [mediate ~ks ~peer ~group ~public] mediates a Diffie-Helmann key exchange with [peer] using
  the public key [public] and group [group]. It gives a pair containing [ks] with an updated or
  new mapping from [peer] in its cache and the shared secret between this and [peer].*)

  val lookup : ks:t -> peer:Peer.t -> Cstruct.t option
  (** [lookup ~ks ~peer] gives an option containing the value [peer] maps to in [ks] if the mapping
   exists. *)

  val lookup' : ks:t -> peer:Peer.t -> (t * Cstruct.t) Lwt.t
  (** [lookup' ~ks ~peer] gives a promise for the pair of [ks], containing a mapping from [peer] in
  its cache, and the shared secret between this and [peer]. In the event that [ks] does not contain
  a mapping from [peer] in its cache, key exchange will occur. *)
end

(** Cryptography service depends on a keying service with the responsibility to encrypt and decrypt 
Cstruct.t messages per peer *)
module CS : sig
  val encrypt : ks:KS.t -> peer:Peer.t -> plaintext:Cstruct.t -> (KS.t * Cstruct.t * Cstruct.t) Lwt.t
  (** [encrypt ~ks ~peer ~plaintext] will encrypt [plaintext] with the shared secret held between
  this and [peer] in [ks]'s cache. This uses [KS.lookup'] to transparently carry out a key exchange
  if such a shared secret doesn't exist in [ks]. The value of calling [encrypt] is a promise to the
  tuple containing [ks] possibly updated such that it contains a shared secret with [peer], the 
  ciphertext result of this encryption and the initial vector of the GCM encryption. *)

  exception Decryption_failed
  (** In the case that decryption fails, this exception is thrown. *)
  
  val decrypt : ks:KS.t -> peer:Peer.t -> ciphertext:Cstruct.t -> iv:Cstruct.t -> Cstruct.t
  (** [decrypt ~ks ~peer ~ciphertext ~iv] will decrypt [ciphertext] using the shared secret held in
  [ks]'s cache between this and [peer] and [iv]. If no shared secret exists [Decryption_failed] is
  raised. *)
end
