open Nocrypto
open Lwt.Infix
open Sexplib
open Cipher_block.AES.CCM

module Serialisation : sig
  exception Deserialisation_failed of string
  val serialise_cstruct     : Cstruct.t -> string
  val deserialise_cstruct   : string    -> Cstruct.t
  val serialise_encrypted   : ciphertext:Cstruct.t -> iv:Cstruct.t -> string
  val deserialise_encrypted : message:string -> Cstruct.t * Cstruct.t
end = struct
  exception Deserialisation_failed of string

  let serialise_cstruct m =
    m
    |> Nocrypto.Base64.encode
    |> Cstruct.to_string

  let deserialise_cstruct m =
    match m |> Cstruct.of_string |> Nocrypto.Base64.decode with
    | Some m' -> m'
    | None    -> raise (Deserialisation_failed m)

  let string_member s j =
    match Yojson.Basic.Util.member s j with
    | `String m -> m
    | _         -> raise (Deserialisation_failed s)

  let deserialise_encrypted ~message =
    let j = Yojson.Basic.from_string message in
    let c = j |> string_member "ciphertext"     |> deserialise_cstruct in
    let i = j |> string_member "initial_vector" |> deserialise_cstruct in
    (c,i)

  let serialise_encrypted ~ciphertext ~iv =
   `Assoc [
      ("ciphertext"     , `String (serialise_cstruct ciphertext));
      ("initial_vector" , `String (serialise_cstruct iv        ))
    ] |> Yojson.Basic.to_string
end

module Signing = Rsa.PSS (Hash.SHA256)

module Keying : sig
  type t
  exception Public_key_not_found of Peer.t
  val empty      : capacity:int -> t
  val invalidate : ks:t -> peer:Peer.t -> t
  val lookup     : ks:t -> peer:Peer.t -> (t * Nocrypto.Rsa.pub) Lwt.t
end = struct
  module V = struct
    type t = Nocrypto.Rsa.pub
    let weight = Nocrypto.Rsa.pub_bits
  end

  module KC = Lru.F.Make(Peer)(V)

  type t = KC.t

  exception Public_key_not_found of Peer.t

  let empty ~capacity = KC.empty capacity

  let invalidate ~ks ~peer = KC.remove peer ks

  let lookup ~ks ~peer =
    match KC.find peer ks with
    | Some (key,ks') -> Lwt.return (ks', key)
    | None           ->
        Http_client.get ~peer ~path:"/peer/pub"
        >|= fun (c,p) ->
        if c=200 then
          let pub = Sexp.of_string p |> Rsa.pub_of_sexp in
          (KC.add peer pub ks),pub
        else raise (Public_key_not_found peer)
end

exception Cannot_decrypt of Cstruct.t * Cstruct.t

let encrypt ~key ~plaintext =
  let key        = of_secret ~maclen:16 key      in
  let nonce      = Rng.generate 12               in
  let ciphertext = encrypt ~key ~nonce plaintext in
  ciphertext, nonce

let decrypt ~key ~ciphertext ~nonce =
  let key = of_secret ~maclen:16 key          in
  match decrypt ~key ~nonce ciphertext      with
  | Some plaintext -> plaintext
  | None           -> raise (Cannot_decrypt (ciphertext,nonce))

let () = Nocrypto_entropy_unix.initialize ()
