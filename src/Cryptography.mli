open Nocrypto
open Lwt
open Lwt.Infix

(* Generic exception to throw when key exchange fails anywhere
 * TODO add information to this to propogate up stack *)
exception Key_exchange_failed

(* Keying service carries out operations on functional key cache *)
module KS : sig
  type t
  (* Internal representation fo the keying service *)

  val empty : capacity:int -> t
  (* Constructor for keying service with an empty cache *)

  val invalidate    : ks:t -> peer:Peer.t -> t 
  (* Returns a keying service with a given peer removed from its key cache*)

  val mediate       : ks:t -> peer:Peer.t -> group:Dh.group -> public:Cstruct.t -> t * Cstruct.t
  (* Given a keying service, peer, DH group and public key, will carry out DH exchange to return a
   * keying service, private key pair
   * TODO just return keying service *)

  val lookup        : ks:t -> peer:Peer.t -> Cstruct.t option
  (* Given a keying service and peer give the shared private key for that peer if it exists *)

  val lookup'       : ks:t -> peer:Peer.t -> (t * Cstruct.t) Lwt.t
  (* Given a keying service and peer, transparently return a promise for a keying service, private 
    * key pair *)
end

(* Cryptography service depends on a keying service and encrypts and decrypts Cstruct.t messages 
 * per Peer.t *)
module CS : sig
  val encrypt : ks:KS.t -> peer:Peer.t -> plaintext:Cstruct.t -> (KS.t * Cstruct.t * Cstruct.t) Lwt.t
  (* Given a keying service, peer and Cstruct plaintext return a promise for a tuple containing a 
   * keying service (with this peer contained), the Cstruct ciphertext and the Cstruct initial 
   * vector
   * TODO type this in a record? *)

  exception Decryption_failed
  (* In the case that decryption fails, this exception is thrown 
   * TODO encode more information*)
  
  val decrypt : ks:KS.t -> peer:Peer.t -> ciphertext:Cstruct.t -> iv:Cstruct.t -> Cstruct.t
  (* Given a keying service, a peer, a cipher text and an initial vector return the decrypted 
   * ciphertext *)
end
