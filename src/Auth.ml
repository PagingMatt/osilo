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
  type tokens = {
    r : bool ;
    w : bool ;
  }

  (* This isn't a BST as I want to be able to do SPM on the file paths *)
  type ft =
    | Node of tokens * string * M.t * (ft list)
    | Leaf

  type ('a, 'b) bst = 
    | Node of ('a * 'b) * ('a, 'b) bst * ('a, 'b) bst
    | Leaf

  type st = (string, ft) bst

  type t  = (Peer.t, st) bst

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
