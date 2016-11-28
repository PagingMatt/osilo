let cstruct = Alcotest.testable (Cstruct.hexdump_pp) (Cstruct.equal) 

module Peer_tests = struct
  let host = "127.0.0.1"
  let port = 8000
  let peer = Peer.create host port

  let peer_builds_with_host () =
    Alcotest.(check string)
      "Checks host is stored and retrieved correctly from Peer"
      host
      (Peer.host peer)

  let peer_builds_with_port () =
    Alcotest.(check int)
      "Checks port is stored and retrieved correctly from Peer"
      port
      (Peer.port peer)

  let tests = [
    ("Correctly builds with host", `Quick, peer_builds_with_host);
    ("Correctly builds with port", `Quick, peer_builds_with_port);
  ]
end

module Coding_tests = struct
  let a = "fooBARfooBARfooBARfooBARfooBARfo"
  let b = "FOObarFOObarFOObarFOObarFOObarFO" 

  let symm_cstruct () =
    let c  = Coding.decode_cstruct a in
    let s  = Coding.encode_cstruct c in
    let c' = Coding.decode_cstruct s in
    Alcotest.(check string) 
      "Checks that starting from a base 64 string, decoding and re-encoding a cstruct is symmetric"
      a s;
    Alcotest.(check cstruct)
      "Checks that starting from a Cstruct, encoding to a base 64 string and decoding back to a cstruct is symmetric"
      c c'

  let symm_message () =
    let c     = Coding.decode_cstruct a     in
    let i     = Coding.decode_cstruct b     in
    let s     = Coding.encode_message c i   in
    let c',i' = Coding.decode_message s     in
    let s'    = Coding.encode_message c' i' in
    Alcotest.(check cstruct)
      "Checks decoding and re-encoding a message produces the same ciphertext"
      c c';
    Alcotest.(check cstruct)
      "Checks decoding and re-encoding a message produces the same initial vector"
      i i';
    Alcotest.(check string)
      "Checks encoding and decoding a message produces the same string to send"
      s s'

  let symm_dh_reply () =
    let p  = Coding.decode_cstruct a   in
    let r  = Coding.encode_kx_reply p  in
    let p' = Coding.decode_kx_reply r  in
    let r' = Coding.encode_kx_reply p' in
    Alcotest.(check cstruct)
      "Checks public key decoded from KX reply is the same as the one it was encoded with"
      p p';
    Alcotest.(check string)
      "Checks kx reply initially encoded is same as decoding and re-encoding reply"
      r r'

  let tests = [
    ("Tests that encoding/decoding cstructs is symmetric", `Quick, symm_cstruct);
    (*("Tests that encoding/decoding DH groups is symmetric", `Quick, symm_group);*)
    ("Tests that encoding/decoding encrypted messages is symmetric", `Quick, symm_message);
    (*("Tests that encoding/decoding a DH key exchange init is symmetric", `Quick, symm_dh_init);*)
    ("Tests that encoding/decoding a DH key exchange reply is symmetric", `Quick, symm_dh_reply)
  ]
end

module Cryptography_tests = struct
  open Cryptography

  let group = Nocrypto.Dh.gen_group 32

  let can_mediate_key_exchange () =
    let ks = KS.empty ~capacity:4 ~master:(Cstruct.of_string "test") in
    let peer = Peer.create "localhost" 8000 in
    let peer_secret,peer_public = Nocrypto.Dh.gen_key group in
    let ks2,my_public = KS.mediate ~ks ~peer ~group ~public:peer_public in
    let my_shared,ks3 = 
      match KS.lookup ~ks:ks2 ~peer with 
      | (Some k, ks4) -> k,ks4
      | (None  , _  ) -> Alcotest.fail "Did not add peer to KS" 
    in
    match Nocrypto.Dh.shared group peer_secret my_public with 
    | None             -> Alcotest.fail "Could not generate shared secret"
    | Some peer_shared -> 
        Alcotest.(check cstruct) "Checks secret computed at peer matches my secret in my KS"
        my_shared peer_shared

  let tests = [
    "Can add peer -> key mapping in an empty KS", `Quick, can_mediate_key_exchange;
  ]
end

let () = 
  Alcotest.run "Osilo Tests" [
    "Peer module"        , Peer_tests.tests;
    "Coding module"      , Coding_tests.tests;
    "Cryptography module", Cryptography_tests.tests; 
  ]
