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
  let b64 = "fooBARfooBARfooBARfooBARfooBARfo"
  
  let symm_cstruct () =
    let c  = Coding.decode_cstruct b64 in
    let s  = Coding.encode_cstruct c   in
    let c' = Coding.decode_cstruct s   in
    Alcotest.(check string) 
      "Checks that starting from a base 64 string, decoding and re-encoding a cstruct is symmetric"
      b64 s;
    Alcotest.(check cstruct)
      "Checks that starting from a Cstruct, encoding to a base 64 string and decoding back to a cstruct is symmetric"
      c c'


  let tests = [
    ("Tests that encoding/decoding cstructs is symmetric", `Quick, symm_cstruct);
    (*("Tests that encoding/decoding DH groups is symmetric", `Quick, symm_group);
    ("Tests that encoding/decoding encrypted messages is symmetric", `Quick, symm_message);
    ("Tests that encoding/decoding DH key exchange init is symmetric", `Quick, symm_dh_init);
    ("Tests that encoding/decoding DH key exchange reply is symmetric", `Quick, symm_dh_reply);
  *)]
end

let () = 
  Alcotest.run "Osilo Tests" [
    "Peer module"  , Peer_tests.tests;
    "Coding module", Coding_tests.tests; 
  ]
