open Core.Std

module Message_variant = struct
  open Messages.Message
  
  let test_of_string () =
    let variants  = [DH_INIT  ; DH_REPLY  ] in
    let strings   = ["dh-init"; "dh-reply"] in
    let strings' = List.map variants ~f:(Messages.Message.to_string) in
    List.iter2_exn strings strings' ~f:(Alcotest.(check string) "Checking deserialised variant")

  let unit_tests = [
    ("Message variants can be deserialised", `Quick, test_of_string)
  ]
end

let () = Alcotest.run "Tests for the Message variants" [("Message variant unit tests", Message_variant.unit_tests)]
