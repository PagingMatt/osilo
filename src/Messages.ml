open Core.Std

module Message = struct
  type t = DH_INIT | DH_REPLY

  exception Unknown_message_type of string

  let of_string s =
    match s with
    | "dh-init"  -> DH_INIT
    | "dh-reply" -> DH_REPLY
    | _          -> raise (Unknown_message_type s)

  let to_string t =
    match t with
    | DH_INIT  -> "dh-init"
    | DH_REPLY -> "dh-reply"
end

module Json = struct
  open Yojson.Basic

  type t = json

  exception Json_deserialised_to_null        of string
  exception Expected_string_member           of string * json
  exception Expected_association_list_member of string * json

  let of_string_not_null s =
    match from_string s with
    | `Null -> raise (Json_deserialised_to_null s)
    | j     -> j

  let member_str j s =
    match Util.member s j with
    | `String m -> m
    | _         -> raise (Expected_string_member (s,j))

  let member_asc j s =
    match Util.member s j with
    | `Assoc m -> `Assoc m
    | _        -> raise (Expected_association_list_member (s,j))

  let string_of_message_body m b =
    let t = Message.to_string m in
    `Assoc [("type",`String t);("body",b)] |> to_string
end
 
module type MESSAGE = sig
  type t
  val serialise : t -> string
  val deserialise : string -> t
end

exception Not_correct_message_type of Message.t * string

module Dh = struct
  open Nocrypto

  exception B64_not_decodeable of string

  let decode_base64 s =
    match Cstruct.of_string s |> Base64.decode with
    | Some d -> d
    | None   -> raise (B64_not_decodeable s)

  module Init : sig
    include MESSAGE
  end = struct
    type t = {
      grp : Dh.group ;
      pub : Cstruct.t ;
    }

    let build_body g p = `Assoc [("grp",`String g);("pub", `String p)]

    let serialise m = 
      let g = Dh.sexp_of_group m.grp |> Sexp.to_string in
      let p = Base64.encode m.pub |> Cstruct.to_string in
      build_body g p |> Json.string_of_message_body DH_INIT
      
    let deserialise s = 
      let json = Json.of_string_not_null s in 
      if (Json.member_str json "type" |> Message.of_string) = DH_INIT
      then 
        let body = Json.member_asc json "body" in
        let grp = Json.member_str body "grp" |> Sexp.of_string |> Dh.group_of_sexp in
        let pub = decode_base64 (Json.member_str body "pub") in
        { grp; pub; }
      else raise (Not_correct_message_type (DH_INIT,s))

  end

  module Reply : sig
    include MESSAGE
  end = struct
    type t = {
      pub : Cstruct.t ;
    }

    let build_body p =
      `Assoc [("pub",`String p)]
   
    let serialise m =
      let p = Base64.encode m.pub |> Cstruct.to_string in
      build_body p |> Json.string_of_message_body DH_REPLY

    let deserialise s = 
      let json = Json.of_string_not_null s in 
      if (Json.member_str json "type" |> Message.of_string) = DH_REPLY
      then 
        let body = Json.member_asc json "body" in
        let pub = decode_base64 (Json.member_str body "pub") in 
        { pub; }
      else raise (Not_correct_message_type (DH_REPLY,s))
  end
end
