open Core.Std
open Messages.Message

let message_types_of_string () =
  let variants  = [DH_INIT  ; DH_REPLY  ] in
  let strings   = ["dh-init"; "dh-reply"] in
  let strings' = List.map variants ~f:(Messages.Message.to_string) in
  List.iter2_exn strings strings' ~f:(Alcotest.(check string) "Checking deserialised variant")

let unit_tests = [
  ("Message variants can be deserialised", `Quick, message_types_of_string)
]

let () = Alcotest.run "Tests for the Messages module" [("Unit tests",unit_tests)]
