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

module Capability = struct
  type token =  R | W

  type t = {
    service : string ;
    path    : string ;
    token   : token  ;
  } with sexp

  let create_read s p =
    {service = s; path = p; token = R;}

  let create_write s p =
    {service = s; path = p; token = W;}

  let to_string c = 
    sexp_of_t c
    |> Sexp.to_string

  let of_string s =
    Sexp.of_string s 
   |> t_of_sexp
end

