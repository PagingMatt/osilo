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

module CS : sig 
  type t 

  val empty : t

  val record_if_most_general : 
    service:t          ->
    permission:Token.t -> 
    macaroon:M.t       -> t

  val find_most_general_capability :
    service:t               ->
    path:string             ->
    permission:Token.t      -> (Token.t * M.t) option
end = struct 
  type t = (Token.t * M.t) File_tree.t

  open Token

  let empty = File_tree.empty

  let location (_,m) = (M.location m |> Core.Std.String.split ~on:'/')

  let select (p1,m1) (p2,m2) = if p2 >> p1 then (p2,m2) else (p1,m1)

  let satisfies permission (t,m) = t >= permission

  let terminate elopt (el,_) = 
    match elopt with 
    | None     -> false
    | Some (el',_) -> el' >= el

  let record_if_most_general ~service ~permission ~macaroon =
    File_tree.insert ~element:(permission,macaroon) ~tree:service ~location ~select ~terminate

  let find_most_general_capability ~service ~path ~permission =
    File_tree.shortest_path_match
      ~tree:service
      ~location:(Core.Std.String.split path ~on:'/') 
      ~satisfies:(satisfies permission)
end

open Token

let record_permissions capability_service permissions = 
  Core.Std.List.fold 
    permissions 
    ~init:capability_service 
    ~f:(fun service -> fun (p,m) -> CS.record_if_most_general ~permission:p ~macaroon:m ~service)

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
  Core.Std.List.fold requests ~init:([],[])
  ~f:(fun (c,n) -> fun (permission,path) -> 
    if covered c (permission,path) then (c,n) else
      CS.find_most_general_capability 
      ~service:capability_service ~path ~permission
    |> begin function 
       | None       -> c,((permission,path)::n)
       | Some (p,m) -> ((p,m)::c),n
       end)    
  |> fun (covered,not_covered) -> (Core.Std.List.map ~f:(fun (p,m) -> m) covered), not_covered

let request_under_verified_path vpaths rpath =
  Core.Std.List.fold vpaths ~init:false ~f:(fun acc -> fun vpath -> acc || (vpath_subsumes_request vpath rpath))

let authorise requests capabilities tok key target service =
  let key' = Coding.encode_cstruct key in
  let verified_capabilities = Core.Std.List.filter capabilities ~f:(verify tok key') in
  let authorised_locations  = Core.Std.List.map verified_capabilities ~f:(M.location) in 
  let path_tree = Core.Std.List.fold ~init:File_tree.empty 
        ~f:(fun tree -> fun element -> File_tree.insert ~element ~tree 
          ~location:(fun path -> Core.Std.String.split path ~on:'/')
          ~select:(fun p -> fun _ -> p)
          ~terminate:(fun o -> fun _ -> match o with | Some e -> true | None -> false)) requests in
  let (authorised_paths,_) = 
    Core.Std.List.fold ~init:([],path_tree) ~f:(fun (paths,tree) -> fun loc -> 
      let content,tree' = File_tree.trim ~tree ~location:(Core.Std.String.split loc ~on:'/')
      in (Core.Std.List.unordered_append content paths),tree') authorised_locations in
  authorised_paths

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
