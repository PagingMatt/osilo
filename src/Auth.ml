module Crypto : Macaroons.CRYPTO = struct
  let hmac ~key message =
    Nocrypto.Hash.SHA512.hmac
      ~key:(Coding.decode_cstruct key)
      (Coding.decode_cstruct message)
    |> Coding.encode_cstruct

  let hash message =
    Nocrypto.Hash.SHA512.digest
      (Coding.decode_cstruct message)
    |> Coding.encode_cstruct

  let encrypt ~key message = 
    let ciphertext,iv = 
      Cryptography.CS.encrypt' 
        ~key:(Coding.decode_cstruct key) 
        ~plaintext:(Coding.decode_cstruct message)
    in Coding.encode_client_message ~ciphertext ~iv

    let decrypt ~key message =
      let ciphertext,iv = Coding.decode_client_message ~message in
        Cryptography.CS.decrypt' 
          ~key:(Coding.decode_cstruct key) 
          ~ciphertext 
          ~iv
      |> Coding.encode_cstruct
end

module M = Macaroons.Make(Crypto)

module CS : sig
  type t
  type token = R | W 
  val create : t
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

  let create = Leaf

  let (@>) t1 t2 =
    match t1 with
    | R  -> false
    | W  -> (t2 = R)

  exception Path_empty

  let insert service permission macaroon =
    let location = M.location macaroon in
    let location' = Core.Std.String.split location ~on:'/' in
    let rec ins path service =
      match path with
      | []    -> raise Path_empty
      | x::[] -> 
          (match service with 
          | Leaf -> Node (x, Some (permission,macaroon), Leaf, Leaf, Leaf) (* When get to singleton at correct level so do normal binary insert *)
          | Node (name, caps, sub, l, r) -> 
              if name > x then Node (name, caps, sub, (ins path l), r) else (* If this node is string greater than target move left in this level *)
              if name < x then Node (name, caps, sub, l, (ins path r)) else (* If this node is string less than target move right in this level*)
              (* Need to determine if this macaroon is more powerful than current *)
              match caps with
              | None -> Node (name, Some (permission,macaroon), sub, l, r)
              | Some (t,m) ->
                  if permission @> t then Node (name, Some (permission,macaroon), sub, l, r)
                  else Node (name, Some (t,m), sub, l, r))
      | y::ys -> 
          match service with (* Above target level so find/insert this level's node and drop to next *)
          | Leaf -> Node (y, Some (permission, macaroon), (ins ys Leaf), Leaf, Leaf) (* If currently bottoming out, need to excavate down *)
          | Node (name,caps,sub,l,r) -> 
              if name > y then Node (name, caps, sub, (ins path l), r) else (* If this node is string greater than target move left in this level *)
              if name < y then Node (name, caps, sub, l, (ins path r)) else (* If this node is string less than target move right in this level*)
              Node (name, caps, (ins ys sub), l, r) (* If this node is string equal to target move down to next level *)
    in ins location' service
end

let record_permissions capability_service permissions = capability_service

let create_service_capability server service (perm,path) =
  let m = M.create 
    ~location:(server#get_address |> Peer.host)
    ~key:(server#get_secret_key |> Cstruct.to_string)
    ~id:(path)
  in let ms = M.add_first_party_caveat m service 
  in M.add_first_party_caveat ms perm

let mint server service permissions =
  Core.Std.List.map permissions ~f:(create_service_capability server service)

let serialise_capabilities capabilities = 
  `List (Core.Std.List.map capabilities ~f:(fun c -> `String (M.serialize c)))
  |> Yojson.Basic.to_string

exception Malformed_data 
 
let deserialise_capabilities capabilities = 
  Yojson.Basic.from_string capabilities 
  |> begin function 
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
     end 