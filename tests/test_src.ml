let cstruct = Alcotest.testable (Cstruct.hexdump_pp) (Cstruct.equal) 

let host = "127.0.0.1"
let port = 6630
let peer = Peer.create host

let host1 = "127.0.0.1"
let peer1 = Peer.create host1

let host2 = "localhost"
let peer2 = Peer.create host2

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
  let server = new Http_server.server' "localhost" (Coding.decode_cstruct key) "localhost" 

  let can_mint_read_macaroons_for_test () =
    let ps = Auth.mint server#get_address server#get_secret_key "test" [("R","test_file.json")] in 
    match ps with
    | (mac::[]) ->
        Alcotest.(check string) "Passed back read token with macaroon" "R" (M.identifier mac);
        Alcotest.(check string) "Macaroon has desired location" "localhost/test/test_file.json" (M.location mac);
        Alcotest.(check bool)   "Macaroon holds correct first party caveat." (verify R key mac) true
    | [] -> Alcotest.fail "Minted no macaroons"
    | _  -> Alcotest.fail "Minted too many/duplicate macaroons"

  let can_mint_write_macaroons_for_test () =
    let ps = Auth.mint server#get_address server#get_secret_key "test" [("W","test_file.json")] in 
    match ps with
    | (mac::[]) ->
        Alcotest.(check string) "Passed back write token with macaroon" "W" (M.identifier mac);
        Alcotest.(check string) "Macaroon has desired location" "localhost/test/test_file.json" (M.location mac);
        Alcotest.(check bool)   "Macaroon holds correct first party caveat." (verify W key mac) true
    | [] -> Alcotest.fail "Minted no macaroons"
    | _  -> Alcotest.fail "Minted too many/duplicate macaroons"

  let write_macaroons_verifies_read_request () =
    let ps = Auth.mint server#get_address server#get_secret_key "test" [("W","test_file.json")] in 
    match ps with
    | (mac::[]) ->
        Alcotest.(check string) "Passed back write token with macaroon" "W" (M.identifier mac);
        Alcotest.(check string) "Macaroon has desired location" "localhost/test/test_file.json" (M.location mac);
        Alcotest.(check bool)   "Verify that can read with this write token." (verify R key mac) true
    | [] -> Alcotest.fail "Minted no macaroons"
    | _  -> Alcotest.fail "Minted too many/duplicate macaroons"

  let minimal_covering_set_of_capabilities () = 
    let token = R in
    let caps = mint server#get_address server#get_secret_key "test" 
      [((token |> string_of_token),"foo/bar"); 
       ((token |> string_of_token),"foo/bar/FOO/BAR")] in
    let paths = [(R,"localhost/test/foo/bar");(R,"localhost/test/foo/bar/FOO/BAR")] in
    let service0 = Auth.CS.empty in
    let service1 = Auth.record_permissions service0 caps in
    let caps2,_ = Auth.find_permissions service1 paths in
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
    Core.Std.List.map paths ~f:(fun p -> (t,Printf.sprintf "127.0.0.1/foo/%s" p))

  let bc_capability = Auth.mint peer (key |> Coding.decode_cstruct) "foo" [("R","a")]
  let cap = 
    match bc_capability with 
    | c::_ -> c

  let tree' = Auth.CS.record_if_most_general ~service:(Auth.CS.empty) ~macaroon:cap

  let tokpaths =
    Core.Std.List.map paths ~f:(fun p -> (s,p))

  let capabilities =
    Auth.mint peer (key |> Coding.decode_cstruct) "foo" tokpaths

  let tree = 
    Core.Std.List.fold ~init:Auth.CS.empty capabilities
      ~f:(fun s' -> fun c' -> Auth.CS.record_if_most_general ~service:s' ~macaroon:c')

  let find_is_deduped () =
    let caps,notf = Auth.find_permissions tree' selection_args in
      Alcotest.(check int) "Checks that best case miminal set is selected"
      (Core.Std.List.length caps) 1;
      Alcotest.(check int) "Checks that best case not found set is empty"
      (Core.Std.List.length notf) 0;
    let caps',notf' = Auth.find_permissions tree selection_args in
      Alcotest.(check int) "Checks that worst case miminal set is selected"
      (Core.Std.List.length caps') number_paths;
      Alcotest.(check int) "Checks that worst case not found set is empty"
      (Core.Std.List.length notf') 0


  let covered_tests () =
    let caps1,_ = Auth.find_permissions tree' selection_args in
    Alcotest.(check bool) "Checks all best case covered"
      (Core.Std.List.fold ~init:true selection_args 
        ~f:(fun b -> fun a -> b && Auth.covered caps1 a))
      true;
    let caps2,_ = Auth.find_permissions tree selection_args in
    Alcotest.(check bool) "Checks all worst case covered"
      (Core.Std.List.fold ~init:true selection_args 
        ~f:(fun b -> fun a -> b && Auth.covered caps2 a))
      true

  let tests = [
    ("Valid tokens can be symmetrically serialised/deserailised.", `Quick, symm_token_serialisation);
    ("Invalid tokens throw on deserialisation.", `Quick, invalid_string_throws);
    ("Checks token 'greater than' infix holds.", `Quick, greater_than_token_tests);
    ("Checks token 'greater than or equal to' infix holds.", `Quick, greater_than_equal_token_tests);
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
    let c     = Coding.decode_cstruct a          in
    let i     = Coding.decode_cstruct b          in
    let s     = Coding.encode_client_message ~ciphertext:c ~iv:i   in
    let c',i' = Coding.decode_client_message s        in
    let s'    = Coding.encode_client_message ~ciphertext:c' ~iv:i' in
    Alcotest.(check cstruct)
      "Checks decoding and re-encoding a client message produces the same ciphertext"
      c c';
    Alcotest.(check cstruct)
      "Checks decoding and re-encoding a client message produces the same initial vector"
      i i';
    Alcotest.(check string)
      "Checks encoding and decoding a client message produces the same string to send"
      s s'

  let symm_peer_message () =
    let c     = Coding.decode_cstruct a          in
    let i     = Coding.decode_cstruct b          in
    let s     = Coding.encode_peer_message ~peer ~ciphertext:c ~iv:i   in
    let p,c',i' = Coding.decode_peer_message s        in
    let s'    = Coding.encode_peer_message ~peer ~ciphertext:c' ~iv:i' in
    Alcotest.(check cstruct)
      "Checks decoding and re-encoding a peer message produces the same ciphertext"
      c c';
    Alcotest.(check cstruct)
      "Checks decoding and re-encoding a peer message produces the same initial vector"
      i i';
    Alcotest.(check string)
      "Checks encoding and decoding a peer message produces the same string to send"
      s s'

  let symm_dh_reply () =
    let p  = Coding.decode_cstruct a   in
    let r  = Coding.encode_kx_reply ~peer ~public:p  in
    let peer',p' = Coding.decode_kx_reply r  in
    let r' = Coding.encode_kx_reply ~peer ~public:p' in
    Alcotest.(check cstruct)
      "Checks public key decoded from KX reply is the same as the one it was encoded with"
      p p';
    Alcotest.(check string)
      "Checks kx reply initially encoded is same as decoding and re-encoding reply"
      r r'

  let tests = [
    ("Tests that encoding/decoding cstructs is symmetric", `Quick, symm_cstruct);
    (*("Tests that encoding/decoding DH groups is symmetric", `Quick, symm_group);*)
    ("Tests that encoding/decoding encrypted client messages is symmetric", `Quick, symm_client_message);
    ("Tests that encoding/decoding encrypted peer messages is symmetric", `Quick, symm_peer_message);
    (*("Tests that encoding/decoding a DH key exchange init is symmetric", `Quick, symm_dh_init);*)
    ("Tests that encoding/decoding a DH key exchange reply is symmetric", `Quick, symm_dh_reply)
  ]
end

module Cryptography_tests = struct
  open Cryptography

  let group = Nocrypto.Dh.gen_group 32

  let can_mediate_key_exchange () =
    let ks = KS.empty ~address:peer ~capacity:4 ~master:(Cstruct.of_string "test") in
    let peer = Peer.create "localhost" in
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

module File_tree_tests = struct
  open Auth
  open Auth.Token

  let key = "fooBARfooBARfooBARfooBARfooBARfo"
  let server = new Http_server.server' "localhost" (Coding.decode_cstruct key) "localhost"

  let location = fun (_,m) -> (M.location m |> Core.Std.String.split ~on:'/')

  let select = fun (p1,m1) -> (fun (p2,m2) -> (if p2 >> p1 then (p2,m2) else (p1,m1)))

  let satisfies = fun permission -> (fun (t,_) -> (t >= permission))

  let terminate elopt (el,_) = 
    match elopt with 
    | None     -> false
    | Some (el',_) -> el' >= el

  let read_macaroon_inserted_into_service_can_be_retrieved () = 
    let token = R in 
    match mint server#get_address server#get_secret_key "test" [((token |> string_of_token),"foo/bar")] with
    | mac::[] -> 
        Alcotest.(check string) "Checks the token is minted correctly"
        (Auth.M.identifier mac) "R";
        Alcotest.(check string) "Checks the minted macaroon has correct location"
        (M.location mac) "localhost/test/foo/bar";
        (let service = File_tree.insert ~element:(token,mac) ~tree:(File_tree.empty) ~location ~select ~terminate in
        match File_tree.shortest_path_match ~tree:service ~location:(["localhost"; "test"; "foo"; "bar"]) ~satisfies:(satisfies token) with
        | Some (_,mac') ->
            Alcotest.(check string) "Checks the stored macaroon is same as the one minted"
            (M.identifier mac') (M.identifier mac);
            Alcotest.(check bool) "Checks that the stored macaroon is valid"
            (verify token key mac') true
        | None -> Alcotest.fail "Could not get Macaroon back out of capability service")
    | _ -> Alcotest.fail "Minting failed" (* Caught in more detail in separate test *)

  let short_circuit_on_find () = 
    let token = R in
    match mint server#get_address server#get_secret_key "test" [((token |> string_of_token),"foo/bar"); ((token |> string_of_token),"foo/bar/FOO/BAR")] with
    | mac1::mac2::[] -> 
        (let service = File_tree.insert ~element:(((Auth.M.identifier mac1) |> token_of_string), mac1) ~tree:(File_tree.empty) ~location ~select ~terminate in
        let service' = File_tree.insert ~element:(((Auth.M.identifier mac2) |> token_of_string), mac2) ~tree:(service) ~location ~select ~terminate in
        match File_tree.shortest_path_match ~tree:service' ~location:(Core.Std.String.split "localhost/test/foo/bar/FOO/BAR" ~on:'/') ~satisfies:(satisfies token) with
        | Some (_,mac') ->
            Alcotest.(check string) "Checks the stored macaroon is same as the one minted"
            (M.identifier mac') (M.identifier mac1);
            Alcotest.(check bool) "Checks that the stored macaroon is valid"
            (verify token key mac') true
        | None -> Alcotest.fail "Could not get short circuiting Macaroon back out of capability service")
    | _ -> Alcotest.fail "Minting failed"

  let can_trim_logged_access_from_tree () =
    let pal = Peer_access_log.empty in
    let host = Peer.create "localhost" in let service = "foo" in 
    let paths = ["bar2";"bar1";"bar3"] in
    let peer = Peer.create "p1" in
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
  let host = Peer.create "192.168.1.86"
  let peer = Peer.create "192.168.1.77"
  let service = "foo"
  let path = "dir/file"

  let access_inserted_into_log_can_be_retrieved () =
    let pal  = Peer_access_log.empty in
    let pal' = Peer_access_log.log pal ~host ~peer ~service ~path in
    match Peer_access_log.delog pal' ~host ~service ~path with
    | p::[],_ ->
        Alcotest.(check string) "Checks the logged peer is the one inserted."
        (Peer.host peer) (Peer.host p);
    | _ -> Alcotest.fail "One single peer access should be logged."

  let access_inserted_into_log_can_be_retrieved_from_node_above () =
    let pal  = Peer_access_log.empty in
    let pal' = Peer_access_log.log pal ~host ~peer ~service ~path in
    match Peer_access_log.delog pal' ~host ~service ~path:"dir" with
    | p::[],_ ->
        Alcotest.(check string) "Checks the logged peer is the one inserted."
        (Peer.host peer) (Peer.host p);
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
      host
      (Peer.host peer)

  let peer_comparison_tests () = 
    if (Peer.compare peer1 peer2)=0 
    then Alcotest.fail "Checks different hosts don't give 0 on comparison"
    else Alcotest.(check int)
      "Checks same hosts give 0 on comparison"
      (Peer.compare peer peer1) 0

  let tests = [
    ("Correctly builds with host", `Quick, peer_builds_with_host);
    ("Checks comparison", `Quick, peer_comparison_tests);
  ]
end

let () = 
  Alcotest.run "Osilo Tests" [
    "API module"         , Api_tests.tests;
    "Auth module"         , Auth_tests.tests;
    "Peer module"        , Peer_tests.tests;
    "Coding module"      , Coding_tests.tests;
    "Cryptography module", Cryptography_tests.tests; 
    "File tree module", File_tree_tests.tests; 
    "Peer access log module", Peer_access_log_tests.tests; 
  ]
