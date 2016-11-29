open Nocrypto
open Lwt
open Lwt.Infix
open Sexplib

let src = Logs.Src.create ~doc:"logger for cryptography" "osilo.cryptography"
module Log = (val Logs.src_log src : Logs.LOG)

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
    Log.info (fun m -> m "Created empty keying service with capacity %d" capacity);
    { address; master; cache = KC.empty capacity}

  let invalidate ~ks ~peer = 
    Log.info (fun m -> m "Invalidating (%s,%d) in keying service's cache" (Peer.host peer) (Peer.port peer));
    {ks with cache = KC.remove peer ks.cache}

  let mediate ~ks ~peer ~group ~public = 
    let secret, public' = Dh.gen_key group in 
    match Dh.shared group secret public with
    | Some shared -> 
        Log.info (fun m -> m "Computed shared key for (%s,%d)." (Peer.host peer) (Peer.port peer)); 
        ({ks with cache = KC.add peer shared ks.cache},public')
    | None        -> 
        Log.err (fun m -> m "Computing shared key for (%s,%d) failed." (Peer.host peer) (Peer.port peer)); 
        raise Key_exchange_failed

  let lookup ~ks ~peer = 
    Log.info (fun m -> m "Looking up (%s,%d) in keying service's cache." (Peer.host peer) (Peer.port peer));
    match KC.find peer ks.cache with
    | Some (k, c) -> 
        Log.info (fun m -> m "Shared secret for (%s,%d) found." (Peer.host peer) (Peer.port peer)); 
        (Some k, {ks with cache = c})
    | None        -> 
        Log.info (fun m -> m "No shared secret for (%s,%d) found." (Peer.host peer) (Peer.port peer)); 
        (None  , ks)

  let lookup' ~ks ~peer =
    match lookup ~ks ~peer with
    | (Some key, ks') -> return (ks', key)
    | (None    , _  ) -> 
        Log.info (fun m -> m "Attempting to get shared secret for (%s,%d)" (Peer.host peer) (Peer.port peer));
        let group = Dh.gen_group 256 in
        let secret, public = Dh.gen_key group in
        let this = ks.address in 
        Log.info (fun m -> m "Initiating the key exchange with (%s,%d)" (Peer.host peer) (Peer.port peer));
        HTTP.init_dh ~this ~peer ~public ~group >|= fun (peer',public') -> 
        match Dh.shared group secret public' with 
        | Some shared -> 
            Log.info (fun m -> m "Transparently computed shared key for (%s,%d)." (Peer.host peer) (Peer.port peer)); 
            ({ks with cache = KC.add peer shared ks.cache}, shared)
        | None        -> 
            Log.err (fun m -> m "Computing shared key for (%s,%d) failed." (Peer.host peer) (Peer.port peer)); 
            raise Key_exchange_failed 
end

module CS : sig
  val encrypt : ks:KS.t -> peer:Peer.t -> plaintext:Cstruct.t -> (KS.t * Cstruct.t * Cstruct.t) Lwt.t
  exception Decryption_failed
  val decrypt : ks:KS.t -> peer:Peer.t -> ciphertext:Cstruct.t -> iv:Cstruct.t -> (KS.t * Cstruct.t)
end = struct
  open Cipher_block.AES.GCM

  let encrypt ~ks ~peer ~plaintext =
    KS.lookup' ks peer >>= fun (k, secret) -> 
      let key     = of_secret secret in 
      let iv      = Rng.generate 256 in 
      let result  = encrypt ~key ~iv plaintext in
      return (k, result.message, iv)

  exception Decryption_failed

  let decrypt ~ks ~peer ~ciphertext ~iv =
    match KS.lookup ks peer with
    | (Some secret, ks') ->
        let key    = of_secret secret in 
        let result = decrypt ~key ~iv ciphertext in
        ks', result.message
    | (None, _)          -> 
        Log.err (fun m -> m "Could not decrypt message.");
        raise Decryption_failed
end

let () = Nocrypto_entropy_unix.initialize ()
