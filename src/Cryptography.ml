open Nocrypto
open Lwt
open Lwt.Infix
open Sexplib

exception Key_exchange_failed

module HTTP : sig 
  val init_dh : this:Peer.t -> peer:Peer.t -> public:Cstruct.t -> group:Dh.group -> (Peer.t * Cstruct.t) Lwt.t
end = struct
  open Coding

  let init_dh ~this ~peer ~public ~group =
    let body = encode_kx_init ~peer:this ~public ~group in
    Http_client.post ~peer ~path:"/kx/init" ~body >|= fun (c,b) -> 
      if c=200 then 
        try decode_kx_reply b with
        | Decoding_failed -> raise Key_exchange_failed
      else raise Key_exchange_failed
end

module KS : sig  
  type t
  val empty      : address:Peer.t -> capacity:int -> master:Cstruct.t -> t  
  val secret     : ks:t -> Cstruct.t
  val invalidate : ks:t -> peer:Peer.t -> t 
  val mediate    : ks:t -> peer:Peer.t -> group:Dh.group -> public:Cstruct.t -> t * Cstruct.t
  val lookup     : ks:t -> peer:Peer.t -> Cstruct.t option * t
  val lookup'    : ks:t -> peer:Peer.t -> (t * Cstruct.t) Lwt.t
end = struct
  module V = struct 
    type t = Cstruct.t
    let weight = Cstruct.len
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

  let mediate ~ks ~peer ~group ~public = 
    let secret, public' = Dh.gen_key group in 
    match Dh.shared group secret public with
    | Some shared -> 
        ({ks with cache = KC.add peer shared ks.cache},public')
    | None        -> 
        raise Key_exchange_failed

  let lookup ~ks ~peer = 
    match KC.find peer ks.cache with
    | Some (k, c) -> (Some k, {ks with cache = c})
    | None        -> (None  , ks)

  let lookup' ~ks ~peer =
    match lookup ~ks ~peer with
    | (Some key, ks') -> return (ks', key)
    | (None    , _  ) -> 
        let group = Dh.gen_group 256 in
        let secret, public = Dh.gen_key group in
        let this = ks.address in 
        HTTP.init_dh ~this ~peer ~public ~group >|= fun (peer',public') -> 
        match Dh.shared group secret public' with 
        | Some shared -> 
            ({ks with cache = KC.add peer shared ks.cache}, shared)
        | None        -> 
            raise Key_exchange_failed 
end

module CS : sig
  val encrypt' : key:Cstruct.t -> plaintext:Cstruct.t -> Cstruct.t * Cstruct.t
  val encrypt  : ks:KS.t -> peer:Peer.t -> plaintext:Cstruct.t -> (KS.t * Cstruct.t * Cstruct.t) Lwt.t
  exception Decryption_failed
  val decrypt' : key:Cstruct.t -> ciphertext:Cstruct.t -> iv:Cstruct.t -> Cstruct.t
  val decrypt  : ks:KS.t -> peer:Peer.t -> ciphertext:Cstruct.t -> iv:Cstruct.t -> (KS.t * Cstruct.t)
end = struct
  open Cipher_block.AES.GCM

  let encrypt' ~key ~plaintext =
    let key    = of_secret key in
    let iv     = Rng.generate 256 in 
    let result = encrypt ~key ~iv plaintext in
    result.message, iv

  let encrypt ~ks ~peer ~plaintext =
    KS.lookup' ks peer >>= fun (k, secret) -> 
      let key        = secret in 
      let result,iv  = encrypt' ~key ~plaintext in
      return (k, result, iv)

  exception Decryption_failed

  let decrypt' ~key ~ciphertext ~iv =
    let key = of_secret key in
    let result = decrypt ~key ~iv ciphertext in
    result.message

  let decrypt ~ks ~peer ~ciphertext ~iv =
    match KS.lookup ks peer with
    | (Some key, ks') -> ks', (decrypt' ~key ~ciphertext ~iv)
    | (None, _) -> raise Decryption_failed
end

let () = Nocrypto_entropy_unix.initialize ()
