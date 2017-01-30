open Nocrypto
open Lwt
open Lwt.Infix
open Sexplib

module KS : sig  
  type t
  val empty      : address:Peer.t -> capacity:int -> master:Cstruct.t -> t  
  val secret     : ks:t -> Cstruct.t
  val invalidate : ks:t -> peer:Peer.t -> t 
  val lookup     : ks:t -> peer:Peer.t -> Cstruct.t option * t
  val lookup'    : ks:t -> peer:Peer.t -> (t * Cstruct.t) Lwt.t
end = struct
  module V = struct 
    type t = Nocrypto.Rsa.pub
    let weight = Nocrypto.Rsa.pub_bits
  end

  module KC = Lru.F.Make(Peer)(V)
  
  type t = {
    address : Peer.t    ;
    master  : Cstruct.t ;
    cache   : KC.t      ;
  }

  let empty ~address ~capacity ~master =
    { address; master; cache = KC.empty capacity}

  let secret ~ks =
    ks.master

  let invalidate ~ks ~peer = 
    {ks with cache = KC.remove peer ks.cache}

  let lookup ~ks ~peer = 
    match KC.find peer ks.cache with
    | Some (k, c) -> (Some k, {ks with cache = c})
    | None        -> (None  , ks)

  let lookup' ~ks ~peer =
    match lookup ~ks ~peer with
    | (Some key, ks') -> return (ks', key)
    | (None    , _  ) -> 
        Http_client.get ~peer ~path:"/peer/rsa/pub" 
        >|= fun (c,body) -> if c=200 then Coding.decode_public_key else raise (Cannot_get_public_key peer)
        >|= fun pub -> ({ks with cache = KC.add peer pub ks.cache}, pub)
end

module CS : sig
  val encrypt_c2p : key:Cstruct.t -> plaintext:Cstruct.t -> Cstruct.t * Cstruct.t
  val encrypt_p2p : ks:KS.t -> peer:Peer.t -> plaintext:Cstruct.t -> (KS.t * Cstruct.t) Lwt.t
  val decrypt_c2p : key:Cstruct.t -> ciphertext:Cstruct.t -> iv:Cstruct.t -> Cstruct.t
  val decrypt_p2p : priv:Nocrypto.Rsa.priv -> ciphertext:Cstruct.t -> Cstruct.t
end = struct
  open Cipher_block.AES.GCM

  let encrypt_c2p ~key ~plaintext =
    let key    = of_secret key in
    let iv     = Rng.generate 256 in 
    let result = encrypt ~key ~iv plaintext in
    result.message, iv

  let encrypt_p2p ~ks ~peer ~plaintext =
    KS.lookup' ks peer >>= fun (k, public) -> 
      let key     = public in 
      let result  = Nocrypto.Rsa.encrypt ~key plaintext in
      return (k, result)

  let decrypt_c2p ~key ~ciphertext ~iv =
    let key = of_secret key in
    let result = decrypt ~key ~iv ciphertext in
    result.message

  let decrypt_p2p ~priv ~ciphertext =
    Nocrypto.Rsa.decrypt ~key:priv ciphertext
end

let () = Nocrypto_entropy_unix.initialize ()
