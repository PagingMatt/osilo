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

let encode_capabilities capabilities =
  let serialised = Core.Std.List.map capabilities ~f:Auth.M.serialize in
  `List (Core.Std.List.map serialised ~f:(fun s -> `String s))

let decode_capabilities capabilities =
  match capabilities with
  | `List j ->
      Core.Std.List.map j
        ~f:(begin function
            | `String s ->
                (Auth.M.deserialize s |>
                 begin function
                 | `Ok c    -> c
                 | `Error _ -> raise (Decoding_failed "Error on deserialisation")
                 end)
            | _ -> raise (Decoding_failed "Wasn't a string")
            end)
  | _ -> raise (Decoding_failed "Wasn't a list")

let pull_out_strings l =
  match l with
  | `List j ->
      Base.List.map j
        ~f:(begin function
            | `String s -> s
            | _         -> raise (Decoding_failed "Wasn't a string")
            end)
  | _ -> raise (Decoding_failed "Wasn't a list of strings")

let decode_file_list_message message =
  message
  |> Yojson.Basic.from_string
  |> pull_out_strings

let encode_file_list_message lst =
  `List (Core.Std.List.map lst ~f:(fun s -> `String s))

let decode_remote_file_list_message message =
  message
  |> Yojson.Basic.from_string
  |> begin function
  | `List rfs -> Core.Std.List.map rfs ~f:decode_json_requested_file
  | _         -> raise (Decoding_failed message)
  end

let decode_file_and_capability_list_message message =
  let json = Yojson.Basic.from_string message in
  let files = Yojson.Basic.Util.member "files" json |> pull_out_strings in
  let capabilities = Yojson.Basic.Util.member "capabilities" json |> decode_capabilities in
  files,capabilities

let pull_out_file_content c =
  match c with
  | `Assoc j -> j
  | _        -> raise (Decoding_failed "Wasn't an association list")

let decode_file_content_and_capability_list_message message =
  let json = Yojson.Basic.from_string message in
  let content = Yojson.Basic.Util.member "contents" json |> pull_out_file_content in
  let capabilities = Yojson.Basic.Util.member "capabilities" json |> decode_capabilities in
  content,capabilities

let decode_file_content_list_message message =
  message
  |> Yojson.Basic.from_string
  |> begin function
     | `Assoc j -> `Assoc j
     | _ -> raise (Decoding_failed message)
     end

let decode_permission_list_message message =
  message
  |> Yojson.Basic.from_string
  |> begin function
     | `Assoc j ->
         Base.List.map j
         ~f:(begin function
         | (permission, `String path) -> ((Auth.Token.token_of_string permission), path)
         | _                          -> raise (Decoding_failed message)
         end)
     | _ -> raise (Decoding_failed message)
     end
