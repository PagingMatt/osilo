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
  val create : t
end = struct
  type t =
    | Node of string * capabilities option * t * t * t
    | Leaf
  and capabilities = token * M.t
  and token = R | W | RW

  let create = Leaf
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