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

  val empty : 
    address:Peer.t ->
    capacity:int ->
    master:Cstruct.t -> t
  (** Constructor for keying service with an empty cache. [empty ~capacity ~master] gives a keying 
  service with an empty cache that can hold [capacity] keys, the private key for which is [master]. 
  *)

  val secret :
    ks:t -> Cstruct.t
  (** [secret ~ks] returns the secret private key for [ks], shared between the clients of this 
  server and this server. *)

  val invalidate : 
    ks:t        -> 
    peer:Peer.t -> t 
  (** [invalidate ~ks ~peer] gives the keying service equal to [ks] without a mapping from [peer]
  in its cache. If [peer] wasn't in [ks]'s cache this is just [ks]. *)

  val lookup : 
    ks:t        -> 
    peer:Peer.t -> Nocrypto.Rsa.pub option * t
  (** [lookup ~ks ~peer] gives an option containing the value [peer] maps to in [ks] if the mapping
   exists, paired with [ks] updated such that [peer]'s mapping is the MRU *)

  val lookup' : 
    ks:t        -> 
    peer:Peer.t -> (t * Nocrypto.Rsa.pub) Lwt.t
  (** [lookup' ~ks ~peer] gives a promise for the pair of [ks], containing a mapping from [peer] in
  its cache, and the shared secret between this and [peer]. In the event that [ks] does not contain
  a mapping from [peer] in its cache, key exchange will occur. *)
end

(** Cryptography service depends on a keying service with the responsibility to encrypt and decrypt 
Cstruct.t messages per peer *)
module CS : sig
  val encrypt_c2p :
    key:Cstruct.t ->
    plaintext:Cstruct.t -> Cstruct.t * Cstruct.t
 (** [encrypt_c2p ~key ~plaintext] encrypts the [plaintext] using the secret [key] and returns a pair
 containing the ciphertext and the inital vector used. *)

  val encrypt_p2p : 
    ks:KS.t             -> 
    peer:Peer.t         -> 
    plaintext:Cstruct.t -> (KS.t * Cstruct.t) Lwt.t
  (** [encrypt_c2p ~ks ~peer ~plaintext] will encrypt [plaintext] with the public key of [peer] in [ks]'s 
  cache. This uses [KS.lookup'] to transparently look up the public key of [peer]. The value of calling [encrypt] 
  is a promise to the tuple containing [ks] possibly updated such that it contains [peer]'s punlic key and the 
  ciphertext result of the RSA encryption. *)

  val decrypt_c2p :
    key:Cstruct.t ->
    ciphertext:Cstruct.t ->
    iv:Cstruct.t -> Cstruct.t
  (** [decrypt' ~key ~ciphertext ~iv] decrypts the [ciphertext] with the private [key], using
  the initial vector [iv]. *)

  val decrypt_p2p : 
    priv:Nocrypto.Rsa.priv -> 
    ciphertext:Cstruct.t   -> Cstruct.t
  (** [decrypt ~priv ~ciphertext] will decrypt [ciphertext] using the private RSA key *)
end
