(** Decoding transmission-suitable (often serialised JSON) [string] types into internal types and
 vice versa. *)

type requested_file = {
  path        : string ;
  check_cache : bool   ;
  write_back  : bool   ;
}
(** Internal type for requested files and caching data about these *)

val encode_json_requested_file : requested_file -> Yojson.Basic.json
(** [encode_json_requested_file file] takes a value of [requested_file] an encodes it into JSON. *)

val decode_json_requested_file : Yojson.Basic.json -> requested_file
(** [decode_json_requested_file json] takes a JSON value [json] and if it can be recoded into a [requested_file]
value this is returned. Otherwise [Decoding_failed] is raised with the [string] representation of [json]
as its parameter. *)

exception Decoding_failed of string
(** Raised if cannot decode a string. *)

val encode_cstruct : Cstruct.t -> string
(** [encode_cstruct m] takes the [Cstruct.t] [m] and encodes it to a base 64 [string] for
transmission. *)

val decode_cstruct : string -> Cstruct.t
(** [decode_cstruct m] takes the base 64 [string] and attempts to decode it into a [Cstruct.t]. *)

val decode_file_list_message : string -> string list
(** [decode_file_list_message msg] takes a [string] and tries to decode it into a [string list] of
file paths. If it fails then [Decoding_failed] is raised. *)

val encode_file_list_message : string list -> Yojson.Basic.json
(** [encode_file_list_message paths] takes a list of files and encodes this into a JSON value to
send.*)

val decode_remote_file_list_message : string -> requested_file list
(** [decode_remote_file_list_message msg] takes a [string] and attempts to decode it into a list
of [requested_files], failing this it raises a [Decoding_failed] with the [msg] as the parameter. *)

val decode_file_and_capability_list_message : string -> (string list) * (Auth.M.t list)
(** [decode_file_and_capability_list_message msg] takes a [string] and attempts to decode it into a
list of file paths paired with a list of capabilities. If it fails to decode it, it raises [Decoding_failed]
with [msg] as the parameter. *)

val decode_file_content_and_capability_list_message : string -> (string * Yojson.Basic.json) list * (Auth.M.t list)
(** [decode_file_content_and_capability_list_message msg] takes [msg] and tries to decode it into a [list]
of [string] file path, [json] content pairs, paired with a [list] of capabilities. If it fails, then
[Decoding_failed] is raised with [msg] as its parameter. *)

val decode_file_content_list_message : string -> [`Assoc of (string * Yojson.Basic.json) list]
(** [decode_file_content_list_message msg] takes a [string] value [msg] and tries to decode this into
a JSON association list, mapping [string] file paths to [json] content. If it fails to do this,
then [Decoding_failed] is raised with [msg] as the parameter. *)

val decode_permission_list_message : string -> (Auth.Token.t * string) list
(** [decode_permission_list_message msg] decodes [msg] into a [list] of pairs of permission tokens and
file paths to then mint into capabilities. *)

val encode_location : source:Peer.t -> service:string -> path:string -> target:Peer.t -> string

val decode_location : string -> Peer.t * string * string * Peer.t

val encode_capabilities : Auth.M.t list -> Yojson.Basic.json
(** Serialises a list of capabilities to accompany a request. This is [Yojson.Basic.json] as it
will then be composed with other JSON. *)

val decode_capabilities : Yojson.Basic.json -> Auth.M.t list
(** Deserialises a JSON collection of capabilities accompanying a request into a list of [M.t]. *)
