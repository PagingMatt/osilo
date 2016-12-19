open Nocrypto

let src = Logs.Src.create ~doc:"logger for osilo authorisation" "osilo.auth"
module Log = (val Logs.src_log src : Logs.LOG)

module Crypto : Macaroons.CRYPTO = struct
  let hmac ~key message =
    Nocrypto.Hash.SHA512.hmac
      ~key:(Cstruct.of_string key)
      (Cstruct.of_string message)
    |> Coding.encode_cstruct

  let hash message =
    Nocrypto.Hash.SHA512.digest
      (Cstruct.of_string message)
    |> Coding.encode_cstruct

  let encrypt ~key message = 
    let ciphertext,iv = 
      Cryptography.CS.encrypt' 
        ~key:(Cstruct.of_string key) 
        ~plaintext:(Cstruct.of_string message)
    in Coding.encode_client_message ~ciphertext ~iv

    let decrypt ~key message =
      let ciphertext,iv = Coding.decode_client_message ~message in
        Cryptography.CS.decrypt' 
          ~key:(Cstruct.of_string key) 
          ~ciphertext 
          ~iv
      |> Coding.encode_cstruct
end

module M = Macaroons.Make(Crypto)

module Token : sig
  type t = R | W 
  exception Invalid_token of string
  val token_of_string : string -> t
  val string_of_token : t -> string
  val (>>) : t -> t -> bool
  val (>=) : t -> t -> bool
end = struct
  type t = R | W 

  exception Invalid_token of string

  let token_of_string t =
    match t with
    | "R" -> R 
    | "W" -> W 
    | _   -> raise (Invalid_token t)

  let string_of_token t =
    match t with
    | R -> "R"
    | W -> "W"

  let (>>) t1 t2 =
    match t1 with
    | R  -> false
    | W  -> (t2 = R)

  let (>=) t1 t2 =
    match t1 with
    | W -> true
    | R -> (t2 = R)
end

open Token

let record_permissions capability_service permissions = 
  let location (_,m) = (M.location m |> Core.Std.String.split ~on:'/') in 
  let select (p1,m1) (p2,m2) = if p2 >> p1 then (p2,m2) else (p1,m1)
  in Core.Std.List.fold 
    permissions 
    ~init:capability_service 
    ~f:(fun tree -> fun (p,m) -> File_tree.insert ~element:(p,m) ~tree ~location ~select)

let create_service_capability host key service (perm,path) =
  let location = Printf.sprintf "%s/%s/%s" (host |> Peer.host) service path in
  let m = 
    Nocrypto_entropy_unix.initialize (); 
    M.create 
      ~location
      ~key:(key |> Coding.encode_cstruct)
      ~id:(Rng.generate 32 |> Coding.encode_cstruct)
  in perm,M.add_first_party_caveat m perm

let mint host key service permissions =
  Core.Std.List.map permissions ~f:(create_service_capability host key service)

let verify tok key mac = (* Verify that I minted this macaroon and it is sufficient for the required operation *)
  M.verify mac ~key ~check:(fun s -> (token_of_string s) >= tok) [] (* Presented a capability at least powerful enough *)

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

let covered caps (perm,path) =
  Core.Std.List.fold caps 
    ~init:false ~f:(fun acc -> fun (p,m) -> 
      (acc || (vpath_subsumes_request (M.location m) path) && (p >= perm)))

let find_permissions capability_service requests =
  let satisfies permission (t,m) = t >= permission
  in Core.Std.List.fold requests ~init:[]
  ~f:(fun acc -> fun (perm,path) -> 
    if covered acc (perm,path) then acc else
      File_tree.shortest_path_match 
      ~tree:capability_service 
      ~location:(Core.Std.String.split path ~on:'/') 
      ~satisfies:(satisfies perm)
    |> begin function 
       | None       -> acc
       | Some (p,m) -> (p,m)::acc
       end)    
  |> Core.Std.List.map ~f:(fun (p,m) -> m)

let request_under_verified_path vpaths rpath =
  Core.Std.List.fold vpaths ~init:false ~f:(fun acc -> fun vpath -> acc || (vpath_subsumes_request vpath rpath))

(* Because of the API definition a collection of requests is always all reads or all writes *)
let authorise requests capabilities tok key target service =
  let key' = Coding.encode_cstruct key in
  let verified_capabilities = Core.Std.List.filter capabilities ~f:(verify tok key') in
  let locations = Core.Std.List.map verified_capabilities ~f:(M.location) in 
  let verified_paths = (* The paths below which it is verified the requester has access of at least [tok] *)
    (Core.Std.List.map locations ~f:(verify_location target service))
    |> Core.Std.List.filter ~f:(fun s -> not(s="")) in 
  Core.Std.List.filter requests ~f:(request_under_verified_path verified_paths)

let serialise_presented_capabilities capabilities =
  `Assoc (Core.Std.List.map capabilities ~f:(fun (p,c) -> (p, `String (M.serialize c))))
  |> Yojson.Basic.to_string

let serialise_request_capabilities capabilities = 
  let serialised = Core.Std.List.map capabilities ~f:M.serialize in
  `List (Core.Std.List.map serialised ~f:(fun s -> `String s))

exception Malformed_data 
 
let deserialise_presented_capabilities capabilities = 
  Yojson.Basic.from_string capabilities 
  |> begin function 
     | `Assoc j ->  
         Core.Std.List.map j 
         ~f:(begin function 
         | p, `String s -> 
             (M.deserialize s |> 
               begin function  
               | `Ok c    -> (token_of_string p),c  
               | `Error _ -> raise Malformed_data 
               end) 
         | _ -> raise Malformed_data  
         end) 
     | _ -> raise Malformed_data 
     end 

let deserialise_request_capabilities capabilities = 
  match capabilities with
  | `List j ->  
      Core.Std.List.map j 
        ~f:(begin function 
            | `String s -> 
                (M.deserialize s |> 
                 begin function  
                 | `Ok c    -> c  
                 | `Error _ -> raise Malformed_data 
                 end) 
            | _ -> raise Malformed_data  
            end) 
  | _ -> raise Malformed_data 
