open Nocrypto
open Lwt
open Lwt.Infix
open Sexplib

exception Key_exchange_failed

module HTTP : sig 
  val init_dh : peer:Peer.t -> public:Cstruct.t -> group:Dh.group -> Cstruct.t Lwt.t
end = struct
  open Coding

  let init_dh ~peer ~public ~group =
    let body = encode_kx_init ~public ~group in
    Http_client.post ~peer ~path:"/kx/init" ~body >|= fun (c,b) -> 
      if c=200 then 
        try decode_kx_reply b with
        | Decoding_failed -> raise Key_exchange_failed
      else raise Key_exchange_failed
end

module KC : sig
  type t
  val empty  : capacity:int -> t
  val remove : t -> Peer.t  -> t
  val lookup : t -> Peer.t  -> Cstruct.t option * t
  val add    : t -> Peer.t  -> Cstruct.t -> t
end = struct 
  module V : sig
    type t = Cstruct.t
    val weight : t -> int
  end = struct 
    type t = Cstruct.t
    let weight c = Cstruct.len c
  end
  module C = Lru.F.Make(Peer)(V)

  type t = {
    cache : C.t ;
  }

  let empty ~capacity = 
    { cache = (C.empty capacity) ; }

  let remove c p =
    {c with cache = (C.remove p c.cache) ; } 

  let lookup c p =
    match C.find p c.cache with
    | Some (v,c') -> (Some v,c')
    | None        -> (None  ,c )

  let add c p k =
    { c with cache = (C.add p k c.cache) ; }
end

module KS : sig  
  type t
  val empty      : capacity:int -> t  
  val invalidate : ks:t -> peer:Peer.t -> t 
  val mediate    : ks:t -> peer:Peer.t -> group:Dh.group -> public:Cstruct.t -> t * Cstruct.t
  val lookup     : ks:t -> peer:Peer.t -> Cstruct.t option * t
  val lookup'    : ks:t -> peer:Peer.t -> (t * Cstruct.t) Lwt.t
end = struct
  type t = {
    cache  : KC.t ;
  }
  
  let empty ~capacity =
    {cache = KC.empty ~capacity}

  let invalidate ~ks ~peer = {cache = KC.remove ks.cache peer}

  let mediate ~ks ~peer ~group ~public = 
    let secret, public' = Dh.gen_key group in 
    match Dh.shared group secret public with
    | Some shared ->
      let added = {cache = KC.add ks.cache peer shared} in 
		  added, public'
    | None -> raise Key_exchange_failed

  let lookup ~ks ~peer = 
    match KC.lookup ks.cache peer with
    | (Some k, c) -> (Some k, {cache = c})
    | (None  , _) -> (None  , ks)

  let lookup' ~ks ~peer =
    match lookup ~ks ~peer with
    | (Some key, ks') -> return (ks', key)
    | (None    , _  ) -> 
        let group = Dh.gen_group 256 in
        let secret, public = Dh.gen_key group in 
        HTTP.init_dh ~peer ~public ~group >|= fun public' -> 
        match Dh.shared group secret public' with 
        | Some shared -> ({cache = KC.add ks.cache peer shared}, shared)
        | None        -> raise Key_exchange_failed 
end

module CS : sig
  val encrypt : ks:KS.t -> peer:Peer.t -> plaintext:Cstruct.t -> (KS.t * Cstruct.t * Cstruct.t) Lwt.t
  exception Decryption_failed
  val decrypt : ks:KS.t -> peer:Peer.t -> ciphertext:Cstruct.t -> iv:Cstruct.t -> (KS.t * Cstruct.t)
end = struct
  open Cipher_block

  let encrypt ~ks ~peer ~plaintext =
    KS.lookup' ks peer >>= fun (k, secret) -> 
      let key     = AES.GCM.of_secret secret in 
      let iv      = Rng.generate 256 in 
      let result  = AES.GCM.encrypt ~key ~iv plaintext in
      return (k, result.message, iv)

  exception Decryption_failed

  let decrypt ~ks ~peer ~ciphertext ~iv =
    match KS.lookup ks peer with
    | (Some secret, ks') ->
        let key    = AES.GCM.of_secret secret in 
        let result = AES.GCM.decrypt ~key ~iv ciphertext in
        ks', result.message
    | (None, _)          -> raise Decryption_failed
end

let () = Nocrypto_entropy_unix.initialize ()
