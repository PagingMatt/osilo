open Nocrypto
open Lwt
open Lwt.Infix
open Sexplib

exception Key_exchange_failed

module HTTP : sig 
  val init_dh : peer:Peer.t -> public:Cstruct.t -> group:Dh.group -> Cstruct.t Lwt.t
end = struct
  open Serialisation

  let build_dh_body ~public ~group = 
    `Assoc [
      ("public", `String (encode public)); 
      ("group",  `String (group  |> Dh.sexp_of_group |> Sexp.to_string))
    ] |> Yojson.Basic.to_string 

  let public_of_dh_reply body = 
      match body |> Yojson.Basic.from_string |> Yojson.Basic.Util.member "public" with
      | `String public -> 
          try decode public with
          | Decoding_failed -> raise Key_exchange_failed
      | _  -> raise Key_exchange_failed

  let init_dh ~peer ~public ~group =
    let body = build_dh_body ~public ~group in
    Http_client.post ~peer ~path:"/kx/init" ~body >|= fun (c,b) -> 
      if c=200 then public_of_dh_reply b
      else raise Key_exchange_failed
end

module KC : sig
  type t
  val empty  : capacity:int -> t
  val remove : t -> Peer.t -> t
  val lookup : t -> Peer.t -> Cstruct.t option
  val add    : t -> Peer.t -> Cstruct.t -> t
end = struct 
  module C = Lru_map.F.Make(Peer)

  type t = {
    cache : Cstruct.t C.t ;
    size  : int           ;
  }

  let empty ~capacity = 
    { cache = C.empty ; size = capacity ;}

  let remove c p =
    {c with cache = C.remove p c.cache} 

  let lookup c p =
    match C.find p c.cache with
    | Some (v,_) -> Some v
    | None       -> None

  let add c p k =
    {c with cache = c.cache
    |> C.add p k  
    |> C.trim c.size}
end

module KS : sig  
  type t
  val empty      : capacity:int -> t  
  val invalidate : ks:t -> peer:Peer.t -> t 
  val mediate    : ks:t -> peer:Peer.t -> group:Dh.group -> public:Cstruct.t -> t * Cstruct.t
  val lookup     : ks:t -> peer:Peer.t -> Cstruct.t option
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

  let lookup ~ks ~peer = KC.lookup ks.cache peer

  let lookup' ~ks ~peer =
    match lookup ~ks ~peer with
    | Some key -> return (ks, key)
    | None     -> 
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
  val decrypt : ks:KS.t -> peer:Peer.t -> ciphertext:Cstruct.t -> iv:Cstruct.t -> Cstruct.t
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
    | Some secret ->
        let key    = AES.GCM.of_secret secret in 
        let result = AES.GCM.decrypt ~key ~iv ciphertext in
        result.message
    | None        -> raise Decryption_failed
end

let () = Nocrypto_entropy_unix.initialize ()
