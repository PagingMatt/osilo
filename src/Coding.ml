exception Decoding_failed

let encode m = 
  m
  |> Nocrypto.Base64.encode
  |> Cstruct.to_string

let decode m =
  match m |> Cstruct.of_string |> Nocrypto.Base64.decode with
  | Some m' -> m'
  | None    -> raise Decoding_failed

let string_member s j = 
  match Yojson.Basic.Util.member s j with
  | `String m -> m
  | _         -> raise Decoding_failed

let encode_encrypted ~ciphertext ~iv = 
  `Assoc [
    ("ciphertext"     , `String (encode ciphertext));
    ("initial_vector" , `String (encode iv        )) 
  ] |> Yojson.Basic.to_string

let decode_encrypted ~message =
  let j = Yojson.Basic.from_string message in
  let c = j |> string_member "ciphertext" |> decode in
  let i = j |> string_member "initial_vector" |> decode in
  (c,i)
