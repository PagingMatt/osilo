open Core.Std
open Lwt.Infix

exception Decoding_failed of string

type requested_file = {
  path        : string ;
  check_cache : bool   ;
  write_back  : bool   ;
}

let encode_cstruct m = 
  m
  |> Nocrypto.Base64.encode
  |> Cstruct.to_string

let decode_cstruct m =
  match m |> Cstruct.of_string |> Nocrypto.Base64.decode with
  | Some m' -> m'
  | None    -> raise (Decoding_failed m)

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
  | _         -> raise (Decoding_failed s)

let int_member s j = 
  match Yojson.Basic.Util.member s j with
  | `Int i -> i
  | _      -> raise (Decoding_failed s)

let bool_member s j = 
  match Yojson.Basic.Util.member s j with
  | `Bool b -> b
  | _      -> raise (Decoding_failed s)

let encode_json_requested_file rf =
  `Assoc [
    ("path"       , `String rf.path);
    ("check_cache", `Bool   rf.check_cache);
    ("write_back" , `Bool   rf.write_back);
  ]

let decode_json_requested_file j =
  {
    path        = string_member  "path"        j;
    check_cache = bool_member    "check_cache" j;
    write_back  = bool_member    "write_back"  j;
  }

(* TODO pull these into a module *)
let tag_ct = "ciphertext"
let tag_iv = "initial_vector"
let tag_pb = "public"
let tag_gr = "group"
let tag_ho = "host"
let tag_me = "message"
let tag_e = "e"
let tag_n = "n"

let encode_public_key pub =
  pub |> Nocrypto.Rsa.sexp_of_pub |> Sexp.to_string

let decode_public_key msg =
  msg |> Sexp.of_string |> Nocrypto.Rsa.pub_of_sexp 

let encode_client_message ~ciphertext ~iv = 
  `Assoc [
    (tag_ct , `String (encode_cstruct ciphertext));
    (tag_iv , `String (encode_cstruct iv        )) 
  ] |> Yojson.Basic.to_string

let decode_client_message ~message =
  let j = Yojson.Basic.from_string message in
  let c = j |> string_member tag_ct |> decode_cstruct in
  let i = j |> string_member tag_iv |> decode_cstruct in
  (c,i)

let encode_peer_message ~peer ~ciphertext = 
  let host = Peer.host peer in
  `Assoc [
    (tag_ho , `String host);
    (tag_ct , `String (encode_cstruct ciphertext));
  ] |> Yojson.Basic.to_string

let decode_peer_message ~message =
  let j = Yojson.Basic.from_string message in
  let h = j |> string_member tag_ho in
  let c = j |> string_member tag_ct |> decode_cstruct in
  let peer = Peer.create h in
  (peer,c)

