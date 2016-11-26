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

let unit_tests = [
  ("Correctly builds with host", `Quick, peer_builds_with_host);
  ("Correctly builds with port", `Quick, peer_builds_with_port);
]

let () = Alcotest.run "Tests for Peer module" [("Unit tests", unit_tests)]
