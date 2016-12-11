open Nocrypto

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

module CS : sig
  type t
  type token = R | W 
  exception Invalid_token of string
  val token_of_string : string -> token
  val string_of_token : token -> string
  val (>>) : token -> token -> bool
  val (>=) : token -> token -> bool
  val create : t
  val insert : token -> M.t -> t -> t
  val shortest_prefix_match : token -> string -> t -> M.t option
end = struct
  type t =
    | Node of string * capabilities option * t * t * t
    | Leaf
    (* A [Node] is of the form (name, (token,macaroon), subtree, left child, right child). For a 
    macaroon which gives a capability on [peer] for [service] at [path], the node with this 
    capability will be in the subtree of [peer], in the subtree of [service] and in the bottom 
    subtree of [path] *)
  and capabilities = token * M.t
  and token = R | W 

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

  let create = Leaf

  let (>>) t1 t2 =
    match t1 with
    | R  -> false
    | W  -> (t2 = R)

  let (>=) t1 t2 =
    match t1 with
    | W -> true
    | R -> (t2 = R)

  exception Path_empty

  let insert permission macaroon service =
    let location = M.location macaroon in
    let location' = Core.Std.String.split location ~on:'/' in
    let rec ins path tree =
      match path with
      | []    -> raise Path_empty
      | x::[] -> 
          (match tree with 
          | Leaf -> Node (x, Some (permission,macaroon), Leaf, Leaf, Leaf) (* When get to singleton at correct level so do normal binary insert *)
          | Node (name, caps, sub, l, r) -> 
              if name > x then Node (name, caps, sub, (ins path l), r) else (* If this node is string greater than target move left in this level *)
              if name < x then Node (name, caps, sub, l, (ins path r)) else (* If this node is string less than target move right in this level*)
              match caps with
              | None -> Node (name, Some (permission,macaroon), sub, l, r)
              | Some (t,m) -> (* Need to determine if this macaroon is more powerful than current *)
                  if permission >> t then Node (name, Some (permission,macaroon), sub, l, r) (* TODO trim sub *)
                  else Node (name, Some (t,m), sub, l, r))
      | y::ys -> 
          match tree with (* Above target level so find/insert this level's node and drop to next *)
          | Leaf -> Node (y, None, (ins ys Leaf), Leaf, Leaf) (* If currently bottoming out, need to excavate down *)
          | Node (name,caps,sub,l,r) -> 
              if name > y then Node (name, caps, sub, (ins path l), r) else (* If this node is string greater than target move left in this level *)
              if name < y then Node (name, caps, sub, l, (ins path r)) else (* If this node is string less than target move right in this level*)
              Node (name, caps, (ins ys sub), l, r) (* If this node is string equal to target move down to next level *)
    in ins location' service

  let shortest_prefix_match permission path service =
    let location' = Core.Std.String.split path ~on:'/' in
    let rec find tree loc =
      match loc with 
      | []    -> None
      | x::[] -> (* At singleton walk down this level in normal bin tree traversal *)
          (match tree with
          | Leaf -> None
          | Node (name, caps, sub, l, r) -> 
              if name > x then find l loc else (* If this node is string greater than target move left in this level *)
              if name < x then find r loc else (* If this node is string less than target move right in this level*)
              match caps with
              | None -> None
              | Some (t,m) -> (* Need to determine if this macaroon is powerful enough *)
                  if t >= permission then Some m else None)
      | y::ys -> (* Find correct member to drop down from and see if can short circuit *)
          match tree with 
          | Leaf -> None
          | Node (name,caps,sub,l,r) -> 
              if name > y then find l loc else (* If this node is string greater than target move left in this level *)
              if name < y then find r loc else (* If this node is string less than target move right in this level*)
              (match caps with
              | None       -> find sub ys (* No capabilities to try to short circuit with *)
              | Some (t,m) -> 
                  if t >= permission 
                  then Some m 
                  else find sub ys)
    in find service location'
end

(* TODO dedup this *)
let find_permissions capability_service requests =
  Core.Std.List.map requests ~f:(fun (perm,path) -> CS.shortest_prefix_match perm path capability_service)

let record_permissions capability_service permissions (* perm,mac pairs *) = 
  Core.Std.List.fold 
    permissions 
    ~init:capability_service 
    ~f:(fun s -> fun (p,m) -> CS.insert p m s)

let create_service_capability server service (perm,path) =
  let location = Printf.sprintf "%s/%s/%s" (server#get_address |> Peer.host) service path in
  let m = 
    Nocrypto_entropy_unix.initialize (); 
    M.create 
      ~location
      ~key:(server#get_secret_key |> Coding.encode_cstruct)
      ~id:(Rng.generate 32 |> Coding.encode_cstruct)
  in perm,M.add_first_party_caveat m perm

let mint server service permissions =
  Core.Std.List.map permissions ~f:(create_service_capability server service)

let verify tok key mac = (* Verify that I minted this macaroon and it is sufficient for the required operation *)
  M.verify mac ~key ~check:(fun s -> (CS.token_of_string s) >= tok) [] (* Presented a capability at least powerful enough *)

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

let request_under_verified_path vpaths rpath =
  Core.Std.List.fold vpaths ~init:false ~f:(fun acc -> fun vpath -> acc || (vpath_subsumes_request vpath rpath))

let authorise requests capabilities tok key target service =
  let key' = Cstruct.to_string key in
  let verified_capabilities = Core.Std.List.filter capabilities ~f:(verify tok key') in
  let locations = Core.Std.List.map verified_capabilities ~f:(M.location) in 
  let verified_paths = (* The paths below which it is verified the requester has access of at least [tok] *)
    (Core.Std.List.map locations ~f:(verify_location target service))
    |> Core.Std.List.filter ~f:(fun s -> not(s="")) in 
  Core.Std.List.filter requests ~f:(request_under_verified_path verified_paths)

let serialise_presented_capabilities capabilities = 
  `Assoc (Core.Std.List.map capabilities ~f:(fun (p,c) -> p, `String (M.serialize c)))
  |> Yojson.Basic.to_string

let serialise_request_capabilities capabilities = 
  let serialised = (Core.Std.List.map capabilities 
    ~f:(
    begin function 
    | Some c -> M.serialize c
    | None -> ""
    end))
  in let serialised' = Core.Std.List.filter serialised ~f:(fun s -> not(s=""))
  in `List (Core.Std.List.map serialised' ~f:(fun s -> `String s))

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
               | `Ok c    -> (CS.token_of_string p),c  
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
