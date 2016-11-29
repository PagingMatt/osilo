open Core.Std
open Lwt.Infix

let src = Logs.Src.create ~doc:"logger for (de)coder" "osilo.coding"
module Log = (val Logs.src_log src : Logs.LOG)

exception Decoding_failed

let encode_cstruct m = 
  m
  |> Nocrypto.Base64.encode
  |> Cstruct.to_string

let decode_cstruct m =
  match m |> Cstruct.of_string |> Nocrypto.Base64.decode with
  | Some m' -> m'
  | None    -> Log.err (fun n -> n "Could not decode base 64 Cstruct from %s" m); raise Decoding_failed

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
  | _         -> Log.err (fun m -> m "Could not find string with key %s in JSON." s); raise Decoding_failed

let int_member s j = 
  match Yojson.Basic.Util.member s j with
  | `Int i -> i
  | _      -> Log.err (fun m -> m "Could not find int with key %s in JSON." s); raise Decoding_failed

(* TODO pull these into a module *)
let tag_ct = "ciphertext"
let tag_iv = "initial_vector"
let tag_pb = "public"
let tag_gr = "group"
let tag_ho = "host"
let tag_po = "port"
let tag_me = "message"

let encode_message ~peer ~ciphertext ~iv = 
  let host = Peer.host peer in
  let port = Peer.port peer in
  `Assoc [
    (tag_ho , `String host);
    (tag_po , `Int    port);
    (tag_ct , `String (encode_cstruct ciphertext));
    (tag_iv , `String (encode_cstruct iv        )) 
  ] |> Yojson.Basic.to_string

let decode_message ~message =
  let j = Yojson.Basic.from_string message in
  let h = j |> string_member tag_ho in
  let p = j |> int_member    tag_po in
  let c = j |> string_member tag_ct |> decode_cstruct in
  let i = j |> string_member tag_iv |> decode_cstruct in
  let peer = Peer.create h p in
  (peer,c,i)

let encode_kx_init ~peer ~public ~group =
  let host = Peer.host peer in
  let port = Peer.port peer in
  let public' = encode_cstruct public in
  let group'  = encode_group   group  in
  Log.info (fun m -> m "Encoding key exchange init message for (%s,%d) with public key '%s' and group '%s'" host port public' group');
  `Assoc [
    (tag_ho, `String host);
    (tag_po, `Int    port);
    (tag_pb, `String public'); 
    (tag_gr, `String group')
  ] |> Yojson.Basic.to_string

let decode_kx_init ~message =
  let j  = Yojson.Basic.from_string message in
  let h  = j |> string_member tag_ho in
  let p  = j |> int_member    tag_po in
  let k  = j |> string_member tag_pb in
  let k' = decode_cstruct k          in
  let g  = j |> string_member tag_gr in
  let g' = decode_group g            in
  let peer = Peer.create h p         in
  Log.info (fun m-> m "Decoded key exchange init message from (%s,%d) with public key '%s' and group '%s'" h p k g);
  (peer,k',g')

let encode_kx_reply ~peer ~public =
  let host = Peer.host peer in
  let port = Peer.port peer in
  let public' = encode_cstruct public in
  Log.info (fun m -> m "Encoding key exchange reply message for (%s,%d) with public key '%s'" host port public');
  `Assoc [
    (tag_ho, `String host);
    (tag_po, `Int    port);
    (tag_pb, `String public')
  ]
  |> Yojson.Basic.to_string

let decode_kx_reply ~message =
  let j = Yojson.Basic.from_string message in
  let h = j |> string_member tag_ho in
  let p = j |> int_member    tag_po in
  let k = j |> string_member tag_pb in
  let k' = decode_cstruct k in 
  let peer = Peer.create h p in
  Log.info (fun m -> m "Decoded key exchange reply message from (%s,%d), their public key is '%s'" h p k);
  (peer,k')

