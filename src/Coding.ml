open Core.Std

exception Decoding_failed

let encode_cstruct m = 
  m
  |> Nocrypto.Base64.encode
  |> Cstruct.to_string

let decode_cstruct m =
  match m |> Cstruct.of_string |> Nocrypto.Base64.decode with
  | Some m' -> m'
  | None    -> raise Decoding_failed

let encode_group g = 
  g
  |> Nocrypto.Dh.sexp_of_group 
  |> Sexp.to_string

let decode_group g = 
  g
  |> Sexp.of_string
  |> Nocrypto.Dh.group_of_sexp

let string_member s j = 
  match Yojson.Basic.Util.member s j with
  | `String m -> m
  | _         -> raise Decoding_failed

(* TODO pull these into a module *)
let tag_ct = "ciphertext"
let tag_iv = "initial_vector"
let tag_pb = "public"
let tag_gr = "group"

let encode_message ~ciphertext ~iv = 
  `Assoc [
    (tag_ct , `String (encode_cstruct ciphertext));
    (tag_iv , `String (encode_cstruct iv        )) 
  ] |> Yojson.Basic.to_string

let decode_message ~message =
  let j = Yojson.Basic.from_string message in
  let c = j |> string_member tag_ct |> decode_cstruct in
  let i = j |> string_member tag_iv |> decode_cstruct in
  (c,i)

let encode_kx_init ~public ~group =
  `Assoc [
    ("public", `String (encode_cstruct public)); 
    ("group",  `String (encode_group   group ))
  ] |> Yojson.Basic.to_string

let decode_kx_init ~message =
  let j = Yojson.Basic.from_string message in
  let p = j |> string_member tag_pb |> decode_cstruct in
  let g = j |> string_member tag_gr |> decode_group   in
  (p,g)

let encode_kx_reply ~public =
  `Assoc [("public", `String (encode_cstruct public))]
  |> Yojson.Basic.to_string

let decode_kx_reply ~message =
  message
  |> Yojson.Basic.from_string 
  |> string_member tag_pb 
  |> decode_cstruct