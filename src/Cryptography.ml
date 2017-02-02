open Nocrypto

open Cipher_block.AES.GCM

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

module Signing = Rsa.PSS (Hash.SHA512)

let encrypt ~key ~plaintext =
  let key    = of_secret key in
  let iv     = Rng.generate 256 in 
  let result = encrypt ~key ~iv plaintext in
  result.message, iv

let decrypt ~key ~ciphertext ~iv =
  let key = of_secret key in
  let result = decrypt ~key ~iv ciphertext in
  result.message

let () = Nocrypto_entropy_unix.initialize ()
