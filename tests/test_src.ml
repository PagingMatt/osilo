let cstruct = Alcotest.testable (Cstruct.hexdump_pp) (Cstruct.equal)

let host = "127.0.0.1"
let port = 6620
let peer = Peer.create host port

let host1 = "127.0.0.1"
let peer1 = Peer.create host1 port

let host2 = "localhost"
let peer2 = Peer.create host2 port

module Api_tests = struct
  open Api

  let pull_out_strings l =
    match l with
    | `List j ->
      Core.Std.List.map j
        ~f:(begin function
            | `String s -> s
            | _         -> raise Malformed_data
          end)
    | _ -> raise Malformed_data

  let get_file_list plaintext =
    Cstruct.to_string plaintext
    |> Yojson.Basic.from_string
    |> pull_out_strings

  let can_get_valid_file_list () =
    let file_list =
      `List [(`String "file_0"); (`String "file_1")]
      |> Yojson.Basic.to_string
      |> Cstruct.of_string in
    let fl = get_file_list file_list in
    Alcotest.(check int)
      "Checks lists are the same length"
      2 (Core.Std.List.length fl);
    match fl with
    | a::b::[] ->
      Alcotest.(check string)
        "Check first item is 'file_0'"
        "file_0" a;
      Alcotest.(check string)
        "Check second item is 'file_1'"
        "file_1" b;
    | _        -> Alcotest.fail "Should be a two item list."

  let empty_file_list_raises_malformed_data () =
    let file_list =
      `List [`Null]
      |> Yojson.Basic.to_string
      |> Cstruct.of_string in
    try
      let _ = get_file_list file_list in
      Alcotest.fail "Did not throw."
    with
    | Api.Malformed_data ->
      let _ = Alcotest.pass in ()

  let malformed_file_list_raises_malformed_data () =
    let file_list =
      `String "Should throw."
      |> Yojson.Basic.to_string
      |> Cstruct.of_string in
    try
      let _ = get_file_list file_list in
      Alcotest.fail "Did not throw."
    with
    | Api.Malformed_data ->
      let _ = Alcotest.pass in ()

  let tests = [
    ("Can extract list of files.", `Quick, can_get_valid_file_list);
    ("Empty list of files throws 'Malformed_data'.", `Quick, empty_file_list_raises_malformed_data);
    ("Malformed data throws 'Malformed_data'.", `Quick, malformed_file_list_raises_malformed_data);
  ]
end

module Auth_tests = struct
  open Auth

  let symm_token_serialisation () =
    let r = "R" in
    let w = "W" in
    let tr = Token.token_of_string r in
    let tw = Token.token_of_string w in
    let r' = Token.string_of_token tr in
    let w' = Token.string_of_token tw in
    Alcotest.(check string)
      "No exception should have been thrown and should have equal string read tokens"
      r r';
    Alcotest.(check string)
      "No exception should have been thrown and should have equal string write tokens"
      w w'

  let invalid_string_throws () =
    let t = "foo" in
    try (let _ = Token.token_of_string t in Alcotest.fail "Tokenised invalid string")
    with
    | Token.Invalid_token s ->
      Alcotest.(check string) "Invalid token throws." s t

  open Token
  let greater_than_token_tests () =
    let r = R in
    let w = W in
    let d = D in
    Alcotest.(check bool) "D is greater than R."     (d >> r) true ;
    Alcotest.(check bool) "W is greater than R."     (w >> r) true ;
    Alcotest.(check bool) "R is not greater than R." (r >> r) false;
    Alcotest.(check bool) "D is greater than W."     (d >> w) true ;
    Alcotest.(check bool) "W is not greater than W." (w >> w) false;
    Alcotest.(check bool) "R is not greater than W." (r >> w) false;
    Alcotest.(check bool) "D is not greater than D." (d >> d) false;
    Alcotest.(check bool) "W is not greater than D." (w >> d) false;
    Alcotest.(check bool) "R is not greater than D." (r >> d) false

  let greater_than_equal_token_tests () =
    let r = R in
    let w = W in
    let d = D in
    Alcotest.(check bool) "D is greater or equal to than R."     (d >= r) true ;
    Alcotest.(check bool) "W is greater or equal to than R."     (w >= r) true ;
    Alcotest.(check bool) "R is greater than or equal to R."     (r >= r) true ;
    Alcotest.(check bool) "D is greater or equal to than W."     (d >= w) true ;
    Alcotest.(check bool) "W is greater than or equal to W."     (w >= w) true ;
    Alcotest.(check bool) "R is not greater than or equal to W." (r >= w) false;
    Alcotest.(check bool) "D is greater or equal to than D."     (d >= d) true ;
    Alcotest.(check bool) "W is not greater than or equal to D." (w >= d) false;
    Alcotest.(check bool) "R is not greater than or equal to D." (r >= d) false

  let key = "fooBARfooBARfooBARfooBARfooBARfo"
  let server = new Http_server.server' "localhost" 6620 (Coding.decode_cstruct key) "localhost" "example_key.pem" "test_cert"
  let delegate = Peer.create "127.0.0.1" 6620
  let delegate2 = Peer.create "foo" 6620

  let if_requester_isnt_delegate_authorise_fails () =
    let ps = Auth.mint ~minter:server#get_address ~key:server#get_secret_key ~service:"test" ~permissions:[(R,"test_file.json")] ~delegate in
    match ps with
    | (mac::[]) ->
      Alcotest.(check bool) "Macaroon doesnt verify for different requester to delegate." (M.verify ~required_service:"test" ~required:R ~key:(Coding.decode_cstruct key) ~this_peer:server#get_address ~requester:delegate2 mac) false;
      Alcotest.(check bool) "Macaroon verifies for same requester as delegate." (M.verify ~required_service:"test" ~required:R ~key:(Coding.decode_cstruct key) ~this_peer:server#get_address ~requester:delegate mac) true
    | [] -> Alcotest.fail "Minted no macaroons"
    | _  -> Alcotest.fail "Minted too many/duplicate macaroons"

  let can_mint_read_macaroons_for_test () =
    let ps = Auth.mint ~minter:server#get_address ~key:server#get_secret_key ~service:"test" ~permissions:[(R,"test_file.json")] ~delegate in
    match ps with
    | (mac::[]) ->
      Alcotest.(check string) "Passed back read token with macaroon" "R" (M.token mac |> Auth.Token.string_of_token);
      Alcotest.(check string) "Macaroon has desired location" "localhost,6620/test/test_file.json" (M.location mac);
      Alcotest.(check bool)   "Macaroon holds correct first party caveat." (M.verify ~required_service:"test" ~required:R ~key:(Coding.decode_cstruct key) ~this_peer:server#get_address ~requester:delegate mac) true
    | [] -> Alcotest.fail "Minted no macaroons"
    | _  -> Alcotest.fail "Minted too many/duplicate macaroons"

  let can_mint_write_macaroons_for_test () =
    let ps = Auth.mint ~minter:server#get_address ~key:server#get_secret_key ~service:"test" ~permissions:[(W,"test_file.json")] ~delegate in
    match ps with
    | (mac::[]) ->
      Alcotest.(check string) "Passed back write token with macaroon" "W" (M.token mac |> Auth.Token.string_of_token);
      Alcotest.(check string) "Macaroon has desired location" "localhost,6620/test/test_file.json" (M.location mac);
      Alcotest.(check bool)   "Macaroon holds correct first party caveat." (M.verify ~required_service:"test" ~required:R ~key:(Coding.decode_cstruct key) ~this_peer:server#get_address ~requester:delegate mac) true
    | [] -> Alcotest.fail "Minted no macaroons"
    | _  -> Alcotest.fail "Minted too many/duplicate macaroons"

  let write_macaroons_verifies_read_request () =
    let ps = Auth.mint ~minter:server#get_address ~key:server#get_secret_key ~service:"test" ~permissions:[(W,"test_file.json")] ~delegate in
    match ps with
    | (mac::[]) ->
      Alcotest.(check string) "Passed back write token with macaroon" "W" (M.token mac |> Auth.Token.string_of_token);
      Alcotest.(check string) "Macaroon has desired location" "localhost,6620/test/test_file.json" (M.location mac);
      Alcotest.(check bool)   "Verify that can read with this write token." (M.verify ~required_service:"test" ~required:R ~key:(Coding.decode_cstruct key) ~this_peer:server#get_address ~requester:delegate mac) true
    | [] -> Alcotest.fail "Minted no macaroons"
    | _  -> Alcotest.fail "Minted too many/duplicate macaroons"

  let minimal_covering_set_of_capabilities () =
    let token = R in
    let caps = mint ~minter:server#get_address ~key:server#get_secret_key ~service:"test"
        ~permissions:[(token,"foo/bar");
                      (token,"foo/bar/FOO/BAR")] ~delegate in
    let paths = [(R,"localhost,6620/test/foo/bar");(R,"localhost,6620/test/foo/bar/FOO/BAR")] in
    let service0 = Auth.CS.empty in
    let service1 = Auth.record_permissions service0 caps in
    let caps2,_ = Auth.find_permissions service1 paths server#get_address "test" in
    Alcotest.(check int) "Two Macaroons should be minted"
      2 (Core.Std.List.length caps);
    Alcotest.(check int) "One Macaroon should be found"
      1 (Core.Std.List.length caps2)

  let number_paths = 200
  let paths =
    Nocrypto_entropy_unix.initialize ();
    Core.Std.List.init number_paths
      ~f:(fun _ ->
          Printf.sprintf "a/%s" (Nocrypto.Rng.generate 32
                                 |> Coding.encode_cstruct |> Core.Std.String.filter ~f:(fun c -> not(c='/')))
        )
  let s = "R"
  let t =  R
  let selection_args =
    Core.Std.List.map paths ~f:(fun p -> (t,Printf.sprintf "127.0.0.1,6620/foo/%s" p))

  let bc_capability = Auth.mint ~minter:peer ~key:(key |> Coding.decode_cstruct) ~service:"foo" ~permissions:[(R,"a")] ~delegate
  let cap =
    match bc_capability with
    | c::_ -> c
    | _    -> assert false

  let tree' = Auth.CS.record_if_most_general ~service:(Auth.CS.empty) ~macaroon:cap

  let tokpaths =
    Core.Std.List.map paths ~f:(fun p -> (t,p))

  let capabilities =
    Auth.mint ~minter:peer ~key:(key |> Coding.decode_cstruct) ~service:"foo" ~permissions:tokpaths ~delegate

  let tree =
    Core.Std.List.fold ~init:Auth.CS.empty capabilities
      ~f:(fun s' -> fun c' -> Auth.CS.record_if_most_general ~service:s' ~macaroon:c')

  let find_is_deduped () =
    let caps,notf = Auth.find_permissions tree' selection_args (Peer.create "127.0.0.1" 6620) "foo" in
    Alcotest.(check int) "Checks that best case miminal set is selected"
      (Core.Std.List.length caps) 1;
    Alcotest.(check int) "Checks that best case not found set is empty"
      (Core.Std.List.length notf) 0;
    let caps',notf' = Auth.find_permissions tree selection_args (Peer.create "127.0.0.1" 6620) "foo" in
    Alcotest.(check int) "Checks that worst case miminal set is selected"
      (Core.Std.List.length caps') number_paths;
    Alcotest.(check int) "Checks that worst case not found set is empty"
      (Core.Std.List.length notf') 0


  let covered_tests () =
    let caps1,_ = Auth.find_permissions tree' selection_args (Peer.create "127.0.0.1" 6620) "foo" in
    Alcotest.(check bool) "Checks all best case covered"
      (Core.Std.List.fold ~init:true selection_args
         ~f:(fun b -> fun a -> b && Auth.covered (Core.Std.List.fold ~init:Auth.CS.empty ~f:(fun cs -> fun m -> Auth.CS.record_if_most_general ~service:cs ~macaroon:m) caps1) a))
      true;
    let caps2,_ = Auth.find_permissions tree selection_args (Peer.create "127.0.0.1" 6620) "foo" in
    Alcotest.(check bool) "Checks all worst case covered"
      (Core.Std.List.fold ~init:true selection_args
         ~f:(fun b -> fun a -> b && Auth.covered (Core.Std.List.fold ~init:Auth.CS.empty ~f:(fun cs -> fun m -> Auth.CS.record_if_most_general ~service:cs ~macaroon:m) caps2) a))
      true

  let tests = [
    ("Valid tokens can be symmetrically serialised/deserailised.", `Quick, symm_token_serialisation);
    ("Invalid tokens throw on deserialisation.", `Quick, invalid_string_throws);
    ("Checks token 'greater than' infix holds.", `Quick, greater_than_token_tests);
    ("Checks token 'greater than or equal to' infix holds.", `Quick, greater_than_equal_token_tests);
    ("Checks requester has to be delegate.", `Quick, if_requester_isnt_delegate_authorise_fails);
    ("Checks location and caveat in minted read macaroon", `Quick, can_mint_read_macaroons_for_test);
    ("Checks location and caveat in minted write macaroon", `Quick, can_mint_write_macaroons_for_test);
    ("Write macaroon can be used for read request", `Quick, write_macaroons_verifies_read_request);
    ("Check is greedy about finding minimal covering set", `Quick, minimal_covering_set_of_capabilities);
    ("Check find is deduplicated", `Quick, find_is_deduped);
    ("Check capabilities cover", `Quick, covered_tests);
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

  let symm_client_message () =
    let open Cryptography.Serialisation in
    let c     = deserialise_cstruct a          in
    let i     = deserialise_cstruct b          in
    let s     = serialise_encrypted ~ciphertext:c ~nonce:i   in
    let c',i' = deserialise_encrypted s        in
    let s'    = serialise_encrypted ~ciphertext:c' ~nonce:i' in
    Alcotest.(check cstruct)
      "Checks decoding and re-encoding a client message produces the same ciphertext"
      c c';
    Alcotest.(check cstruct)
      "Checks decoding and re-encoding a client message produces the same initial vector"
      i i';
    Alcotest.(check string)
      "Checks encoding and decoding a client message produces the same string to send"
      s s'

  let tests = [
    ("Tests that encoding/decoding cstructs is symmetric", `Quick, symm_cstruct);
    (*("Tests that encoding/decoding DH groups is symmetric", `Quick, symm_group);*)
    ("Tests that encoding/decoding encrypted client messages is symmetric", `Quick, symm_client_message);
  ]
end

module File_tree_tests = struct
  open Auth
  open Auth.Token

  let key = "fooBARfooBARfooBARfooBARfooBARfo"
  let server = new Http_server.server' "localhost" 6620 (Coding.decode_cstruct key) "localhost" "example_key.pem" "test_cert"
  let delegate = Peer.create "127.0.0.1" 6620

  let location = fun (_,m) -> (M.location m |> Core.Std.String.split ~on:'/')

  let select = fun (p1,m1) -> (fun (p2,m2) -> (if p2 >> p1 then (p2,m2) else (p1,m1)))

  let satisfies = fun permission -> (fun (t,_) -> (t >= permission))

  let terminate elopt (el,_) =
    match elopt with
    | None     -> false
    | Some (el',_) -> el' >= el

  let read_macaroon_inserted_into_service_can_be_retrieved () =
    let token = R in
    match mint ~minter:server#get_address ~key:server#get_secret_key ~service:"test" ~permissions:[(token,"foo/bar")] ~delegate with
    | mac::[] ->
      Alcotest.(check string) "Checks the token is minted correctly"
        (Auth.M.token mac |> Auth.Token.string_of_token) "R";
      Alcotest.(check string) "Checks the minted macaroon has correct location"
        (M.location mac) "localhost,6620/test/foo/bar";
      (let service = File_tree.insert ~element:(token,mac) ~tree:(File_tree.empty) ~location ~select ~terminate in
       match File_tree.shortest_path_match ~tree:service ~location:(["localhost,6620"; "test"; "foo"; "bar"]) ~satisfies:(satisfies token) with
       | Some (_,mac') ->
         Alcotest.(check string) "Checks the stored macaroon is same as the one minted"
           (Auth.M.token mac' |> Auth.Token.string_of_token) (Auth.M.token mac |> Auth.Token.string_of_token);
         Alcotest.(check bool) "Checks that the stored macaroon is valid"
           (Auth.M.verify ~required_service:"test" ~required:token ~key:(Coding.decode_cstruct key) ~this_peer:server#get_address ~requester:delegate mac') true
       | None -> Alcotest.fail "Could not get Macaroon back out of capability service")
    | _ -> Alcotest.fail "Minting failed" (* Caught in more detail in separate test *)

  let short_circuit_on_find () =
    let token = R in
    match mint ~minter:server#get_address ~key:server#get_secret_key ~service:"test" ~permissions:[(token,"foo/bar"); (token,"foo/bar/FOO/BAR")] ~delegate with
    | mac1::mac2::[] ->
      (let service = File_tree.insert ~element:((Auth.M.token mac1), mac1) ~tree:(File_tree.empty) ~location ~select ~terminate in
       let service' = File_tree.insert ~element:((Auth.M.token mac2), mac2) ~tree:(service) ~location ~select ~terminate in
       match File_tree.shortest_path_match ~tree:service' ~location:(Core.Std.String.split "localhost,6620/test/foo/bar/FOO/BAR" ~on:'/') ~satisfies:(satisfies token) with
       | Some (_,mac') ->
         Alcotest.(check string) "Checks the stored macaroon is same as the one minted"
           (Auth.M.token mac' |> Auth.Token.string_of_token) (Auth.M.token mac1 |> Auth.Token.string_of_token);
         Alcotest.(check bool) "Checks that the stored macaroon is valid"
           (Auth.M.verify ~required_service:"test" ~required:token ~key:(Coding.decode_cstruct key) ~this_peer:server#get_address ~requester:delegate mac') true
       | None -> Alcotest.fail "Could not get short circuiting Macaroon back out of capability service")
    | _ -> Alcotest.fail "Minting failed"

  let can_trim_logged_access_from_tree () =
    let pal = Peer_access_log.empty in
    let host = Peer.create "localhost" 6620 in let service = "foo" in
    let paths = ["bar2";"bar1";"bar3"] in
    let peer = Peer.create "p1" 6620 in
    let pal' =
      Core.Std.List.fold paths ~init:pal
        ~f:(fun p -> fun path -> Peer_access_log.log p ~host ~service ~peer ~path) in
    Alcotest.(check int) "Checks can get peer back out for path we delete"
      (Core.Std.List.length (Peer_access_log.delog pal' ~host ~service ~path:"bar2" |> fun (ps,log') -> ps)) 1;
    Alcotest.(check int) "Checks can get peer back out for left path"
      (Core.Std.List.length (Peer_access_log.delog pal' ~host ~service ~path:"bar1" |> fun (ps,log') -> ps)) 1;
    Alcotest.(check int) "Checks can get peer back out for right path"
      (Core.Std.List.length (Peer_access_log.delog pal' ~host ~service ~path:"bar3" |> fun (ps,log') -> ps)) 1;
    let _,pal'' = Peer_access_log.delog pal' ~host ~service ~path:"bar2" in
    Alcotest.(check int) "Checks cannot get peer back out for path we deleted"
      (Core.Std.List.length (Peer_access_log.delog pal'' ~host ~service ~path:"bar2" |> fun (ps,log') -> ps)) 0;
    Alcotest.(check int) "Checks can get peer back out for what is still left path"
      (Core.Std.List.length (Peer_access_log.delog pal'' ~host ~service ~path:"bar1" |> fun (ps,log') -> ps)) 1;
    Alcotest.(check int) "Checks can get peer back out for what is now root"
      (Core.Std.List.length (Peer_access_log.delog pal'' ~host ~service ~path:"bar3" |> fun (ps,log') -> ps)) 1

  let tests = [
    ("Can add Macaroon to Capabilities Service and get it out again", `Quick, read_macaroon_inserted_into_service_can_be_retrieved);
    ("Will short circuit on find for Macaroon", `Quick, short_circuit_on_find);
    ("Checks trimming tree works as expected", `Quick, can_trim_logged_access_from_tree)
  ]
end

module Peer_access_log_tests = struct
  let host = Peer.create "192.168.1.86" 6620
  let peer = Peer.create "192.168.1.77" 6620
  let service = "foo"
  let path = "dir/file"

  let access_inserted_into_log_can_be_retrieved () =
    let pal  = Peer_access_log.empty in
    let pal' = Peer_access_log.log pal ~host ~peer ~service ~path in
    match Peer_access_log.delog pal' ~host ~service ~path with
    | p::[],_ ->
      Alcotest.(check string) "Checks the logged peer is the one inserted."
        (Peer.string_of_t peer) (Peer.string_of_t p);
    | _ -> Alcotest.fail "One single peer access should be logged."

  let access_inserted_into_log_can_be_retrieved_from_node_above () =
    let pal  = Peer_access_log.empty in
    let pal' = Peer_access_log.log pal ~host ~peer ~service ~path in
    match Peer_access_log.delog pal' ~host ~service ~path:"dir" with
    | p::[],_ ->
      Alcotest.(check string) "Checks the logged peer is the one inserted."
        (Peer.string_of_t peer) (Peer.string_of_t p);
    | _ -> Alcotest.fail "One single peer access should be logged."

  let tests = [
    ("Can add access to Peer Access Log and get it out again", `Quick, access_inserted_into_log_can_be_retrieved);
    ("Can add access to Peer Access Log and get it out again from flattening higher node", `Quick, access_inserted_into_log_can_be_retrieved_from_node_above);
  ]
end

module Peer_tests = struct
  let peer_builds_with_host () =
    Alcotest.(check string)
      "Checks host is stored and retrieved correctly from Peer"
      host (Peer.host peer)

  let peer_builds_with_port () =
    Alcotest.(check int)
      "Checks port is stored and retrieved correctly from Peer"
      port (Peer.port peer)

  let peer_comparison_tests () =
    if (Peer.compare peer1 peer2)=0
    then Alcotest.fail "Checks different hosts don't give 0 on comparison"
    else Alcotest.(check int)
        "Checks same hosts give 0 on comparison"
        (Peer.compare peer peer1) 0

  let serialising_symmetric_to_deserialising () =
    let s1 = Peer.string_of_t peer in
    let p2 = Peer.t_of_string s1   in
    let s2 = Peer.string_of_t p2   in
    Alcotest.(check string) "Checks serialised forms are the same" s1 s2;
    Alcotest.(check int) "Checks deserialised forms are the same"
      (Peer.compare peer p2) 0

  let tests = [
    ("Correctly builds with host", `Quick, peer_builds_with_host);
    ("Correctly builds with port", `Quick, peer_builds_with_port);
    ("Checks comparison", `Quick, peer_comparison_tests);
    ("Checks serialisation is symmetric", `Quick, serialising_symmetric_to_deserialising);
  ]
end

module Cryptography_tests = struct
  open Sexplib

  let sign_verify_test () =
    let server = new Http_server.server' "localhost" 6620 (Coding.decode_cstruct "testtesttesttesttesttesttesttest") "localhost" "example_key.pem" "test_cert" in
    let message = "foo bar." in
    let sign = Api.sign message server in
    let verified = Cryptography.Signing.verify
        ~key:server#get_public_key ~signature:(Cstruct.of_string sign) (Cstruct.of_string message) in
    Alcotest.(check bool)
      "Checks verifies"
      verified true

  let verify_test () =
    let message = "[\"MwAAAGxvY2F0aW9uIG9zaWxvLm1hdHRhaGFycmlzb24uY29tL2Jsb2dnZXIvcG9zdHMKEQAAAGlkZW50aWZpZXIgUgpnAAAAc2lnbmF0dXJlIE1iNk1HWDQ5MnZiRGZaajFQbk11OWJHUWtkM29ZazRzZmNYRzV1dy9FQWlYYytPdTFWZk9tS1F6VmNoeUhuSWYydWtZelZqMjJPQktzemc1bTZvVTF3PT0K\"]" in
    let signature = "MhRedYvVgP2QH/o1LFXnbhLin+xzX87bwp45CHij5lYDTyThE2FbGL1t12vdNUmRO53XRw0R1uvA3Qo0Eji0YffKxqhdChYbt71OI9wYNH281T1IqQrxXXOnQFQGOKzNEZrbhQzT3E60FgiM9DLe2DcwMTfoaavEv+AJpSJ6CDOgUPb1ifB+0zwaiirSRob4XZNmTQUE9MqiuwS/fZ50PEkMAsnUhzbOn6wiWw2SiK4o2DrsfNYNb01mUyfy07lcQJZtYrKPaaML9MdLh7kYpOt94UQDYpX8t5x1JAS4dzmeNEAYKboy7vFzspqRWQqEdKg7DTdsD/nmrf/ZHPr7LA=="
                    |> Cryptography.Serialisation.deserialise_cstruct in
    let key = "((e 65537)(n 24254383114293383472285170238421455864401353152668305030780651190114163719337426933403646687495743501728407070026247791023751253719276859573226372935586077561211287586961643164829810071680875868930569237295965307606470152492239405694424417863645980000881058038785966251765474666005596490920472551579179499318384229885967996918173532080385239428255347061480393526840976083437738174290808472480644023129478974522996729201300482738251241255622337361210893932552729250794709526849726043809809044114843555854457212449994868172941952261923296641260011714761627890659260622648741711855550009000828936974812726635006566009271))"
              |> Sexp.of_string |> Nocrypto.Rsa.pub_of_sexp in
    let verified = Cryptography.Signing.verify
        ~key ~signature (Cstruct.of_string message) in
    Alcotest.(check bool)
      "Checks verifies"
      verified true

  let tests = [
    ("Signing and verifying is symmetric", `Quick, sign_verify_test);
    ("Verifying real world example", `Quick, verify_test);
  ]
end

let () =
  Alcotest.run "Osilo Tests" [
    "API module"         , Api_tests.tests;
    "Auth module"         , Auth_tests.tests;
    "Crypto module"         , Cryptography_tests.tests;
    "Peer module"        , Peer_tests.tests;
    "Coding module"      , Coding_tests.tests;
    "File tree module", File_tree_tests.tests;
    "Peer access log module", Peer_access_log_tests.tests;
  ]
