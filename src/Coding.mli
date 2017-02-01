(** Decoding transmission-suitable (often serialised JSON) [string] types into internal types and
 vice versa. *)

type requested_file = {
  path        : string ;
  check_cache : bool   ;
  write_back  : bool   ;
}
(** Internal type for requested files and caching data about these *)

val encode_json_requested_file : requested_file -> Yojson.Basic.json

val decode_json_requested_file : Yojson.Basic.json -> requested_file

exception Decoding_failed of string
(** Raised if cannot decode a string. *)

val encode_cstruct : Cstruct.t -> string 
(** [encode_cstruct m] takes the [Cstruct.t] [m] and encodes it to a base 64 [string] for 
transmission. *)

val decode_cstruct : string -> Cstruct.t
(** [decode_cstruct m] takes the base 64 [string] and attempts to decode it into a [Cstruct.t]. *)

val encode_client_message : ciphertext:Cstruct.t -> iv:Cstruct.t -> string
(** [encode_client_message ~ciphertext ~iv] constructs the JSON string encoding [ciphertext] and
[iv] to send between server and client. *)

val decode_client_message : message:string -> Cstruct.t * Cstruct.t 
(** [decode_peer_message ~message] takes the JSON string [message] which encodes an initial vector and
ciphertext, sent by some client. The result is the pair containing these. *)

val decode_file_list_message : string -> string list

val encode_file_list_message : string list -> Yojson.Basic.json

val decode_remote_file_list_message : string -> requested_file list

val decode_file_and_capability_list_message : string -> (string list) * (Auth.M.t list)

val decode_file_content_and_capability_list_message : string -> (string * Yojson.Basic.json) list * (Auth.M.t list)

val decode_file_content_list_message : string -> [`Assoc of (string * Yojson.Basic.json) list]

val decode_permission_list_message : string -> (Auth.Token.t * string) list

val encode_capabilities : Auth.M.t list -> Yojson.Basic.json
(** Serialises a list of capabilities to accompany a request. This is [Yojson.Basic.json] as it
will then be composed with other JSON. *)

val decode_capabilities : Yojson.Basic.json -> Auth.M.t list
(** Deserialises a JSON collection of capabilities accompanying a request into a list of [M.t]. *)
