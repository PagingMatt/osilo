open Nocrypto

let src = Logs.Src.create ~doc:"logger for osilo authorisation" "osilo.auth"
module Log = (val Logs.src_log src : Logs.LOG)

module Crypto : Macaroons.CRYPTO = struct
  open Cryptography.Serialisation

  let hmac ~key message =
    Nocrypto.Hash.SHA512.hmac
      ~key:(Cstruct.of_string key)
      (Cstruct.of_string message)
    |> serialise_cstruct

  let hash message =
    Nocrypto.Hash.SHA512.digest
      (Cstruct.of_string message)
    |> serialise_cstruct

  let encrypt ~key message =
    let ciphertext,nonce =
      Cryptography.encrypt
        ~key:(Cstruct.of_string key)
        ~plaintext:(Cstruct.of_string message)
    in
    serialise_encrypted ~ciphertext ~nonce

  let decrypt ~key message =
    let ciphertext,nonce = deserialise_encrypted ~message in
      Cryptography.decrypt
        ~key:(Cstruct.of_string key)
        ~ciphertext
        ~nonce
    |> serialise_cstruct

  let () = Nocrypto_entropy_unix.initialize ()
end

module Token : sig
  type t = R | W | D
  exception Invalid_token of string
  val token_of_string : string -> t
  val string_of_token : t -> string
  val (>>) : t -> t -> bool
  val (>=) : t -> t -> bool
end = struct
  type t = R | W | D

  exception Invalid_token of string

  let token_of_string t =
    match t with
    | "R" -> R
    | "W" -> W
    | "D" -> D
    | _   -> raise (Invalid_token t)

  let string_of_token t =
    match t with
    | R -> "R"
    | W -> "W"
    | D -> "D"

  let (>>) t1 t2 =
    match t1 with
    | R  -> false
    | W  -> (t2 = R)
    | D  -> (t2 = W) || (t2 = R)

  let (>=) t1 t2 =
    match t1 with
    | D -> true
    | W -> (t2 = W) || (t2 = R)
    | R -> (t2 = R)
end

module M : sig
  type t
  val create :
    source:Peer.t  ->
    service:string ->
    path:string    ->
    target:Peer.t  ->
    token:Token.t  ->
    key:Cstruct.t  -> t
  val target : t -> Peer.t
  val source : t -> Peer.t
  val service : t -> string
  val path : t -> string
  val location : t -> string
  val token : t -> Token.t
  val verify : t ->
    key:Cstruct.t    ->
    required:Token.t -> bool
  val string_of_t : t -> string
  val t_of_string : string -> t
end = struct
  module Mac = Macaroons.Make(Crypto)

  type t = Mac.t

  let create ~source ~service ~path ~target ~token ~key =
    let open Cryptography.Serialisation in
    let location = Coding.encode_location source service path target in
    Mac.create
      ~location
      ~key:(key |> serialise_cstruct)
      ~id:(Token.string_of_token token)

  let source macaroon =
    let s,_,_,_ = Mac.location macaroon |> Coding.decode_location in s

  let service macaroon =
    let _,s,_,_ = Mac.location macaroon |> Coding.decode_location in s

  let path macaroon =
    let _,_,p,_ = Mac.location macaroon |> Coding.decode_location in p

  let target macaroon =
    let _,_,_,t = Mac.location macaroon |> Coding.decode_location in t

  let location macaroon =
    let source,service,path,_ = Mac.location macaroon |> Coding.decode_location in
    Printf.sprintf "%s/%s/%s" (source |> Peer.host) service path

  let token macaroon =
    Mac.identifier macaroon
    |> Token.token_of_string

  let verify macaroon ~key ~required =
    let open Cryptography.Serialisation in
    let open Token in
    Mac.verify macaroon ~key:(serialise_cstruct key) ~check:(fun _ -> true) []
    && (token_of_string (Mac.identifier macaroon)) >= required

  let t_of_string s =
    Mac.deserialize s
    |> begin function
      | `Ok m    -> m
      | `Error (e,_) -> raise (Coding.Decoding_failed
          (Printf.sprintf "Deserialising Macaroon failed with %d" e))
    end

  let string_of_t = Mac.serialize
end

module CS : sig
  type t

  val empty : t

  val record_if_most_general :
    service:t          ->
    macaroon:M.t       -> t

  val find_most_general_capability :
    service:t               ->
    path:string             ->
    permission:Token.t      -> M.t option

  val all_capabilities : t -> M.t list
end = struct
  type t = M.t File_tree.t

  open Token

  let empty = File_tree.empty

  let location m = (Peer.host (M.source m))::(M.service m)::(M.path m |> Core.Std.String.split ~on:'/')

  let select m1 m2 =
    if (M.token m2) >> (M.token m1)
    then m2 else m1

  let satisfies permission m = (M.token m) >= permission

  let terminate elopt el =
    match elopt with
    | None     -> false
    | Some el' -> (M.token el') >= (M.token el)

  let record_if_most_general ~service ~macaroon =
    File_tree.insert ~element:macaroon ~tree:service ~location ~select ~terminate

  let find_most_general_capability ~service ~path ~permission =
    File_tree.shortest_path_match
      ~tree:service
      ~location:(Core.Std.String.split path ~on:'/')
      ~satisfies:(satisfies permission)

  let all_capabilities cs = File_tree.flatten ~tree:cs
end

open Token

let record_permissions capability_service permissions =
  Core.Std.List.fold
    permissions
    ~init:capability_service
    ~f:(fun service -> fun m -> CS.record_if_most_general ~macaroon:m ~service)

let mint host key service permissions delegate =
  Core.Std.List.map permissions ~f:(fun (token,path) ->
      M.create ~source:host ~service ~path ~target:delegate ~token ~key)

let verify_location target service l =
  match Core.Std.String.split l ~on:'/' with
  | x::y::zs -> if (x=Peer.host target) && (y=service) then Core.Std.String.concat ~sep:"/" zs else ""
  | _        -> ""

let vpath_subsumes_request vpath rpath =
  let vpath' = Core.Std.String.split vpath ~on:'/' in
  let rpath' = Core.Std.String.split rpath ~on:'/' in
  let rec walker v r =
    match v with
    | []    -> true
    | x::xs ->
      match r with
      | []    -> false
      | y::ys -> x=y && (walker xs ys)
  in walker vpath' rpath'

let covered caps (permission,path) =
  match CS.find_most_general_capability ~service:caps ~path ~permission with
  | Some _ -> true
  | None   -> false

let find_permissions capability_service requests peer service =
  Core.Std.List.fold requests ~init:(CS.empty,[])
  ~f:(fun (c,n) -> fun (permission,path) ->
    if covered c (permission,path) then (c,n) else
      CS.find_most_general_capability
      ~service:capability_service ~path ~permission
    |> begin function
       | None   -> c,((permission,path)::n)
       | Some m -> (CS.record_if_most_general ~service:c ~macaroon:m),n
       end)
    |> fun (covered,not_covered) ->
         (CS.all_capabilities covered),not_covered

let request_under_verified_path vpaths rpath =
  Core.Std.List.fold vpaths ~init:false ~f:(fun acc -> fun vpath -> acc || (vpath_subsumes_request vpath rpath))

let authorise requests capabilities tok key target service =
  let open Cryptography.Serialisation in
  let key' = serialise_cstruct key in
  let verified_capabilities = Core.Std.List.filter capabilities ~f:(M.verify ~required:tok ~key:(deserialise_cstruct key')) in
  let authorised_locations  = Core.Std.List.map verified_capabilities ~f:(M.location) in
  let path_tree = Core.Std.List.fold ~init:File_tree.empty
        ~f:(fun tree -> fun element ->
          File_tree.insert ~element ~tree
          ~location:(fun path -> Core.Std.String.split
            (Printf.sprintf "%s/%s/%s" (Peer.host target) service path) ~on:'/')
          ~select:(fun p -> fun _ -> p)
          ~terminate:(fun o -> fun _ -> match o with | Some e -> true | None -> false)) requests in
  let authorised_paths =
    Core.Std.List.fold ~init:[] ~f:(fun (paths) -> fun loc ->
      let content = File_tree.flatten_below ~tree:path_tree ~location:(Core.Std.String.split loc ~on:'/')
      in (Core.Std.List.unordered_append content paths)) authorised_locations in
  authorised_paths
