open Core.Std
open Cryptography
open Silo
open Lwt.Infix

exception Malformed_data
exception Fetch_failed of Peer.t
exception Path_info_exn of string

let src = Logs.Src.create ~doc:"logger for osilo API" "osilo.api"
module Log = (val Logs.src_log src : Logs.LOG)

class ping s = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  method content_types_provided rd = 
    Wm.continue [("text/plain", self#to_text)] rd

  method content_types_accepted rd = Wm.continue [] rd
  
  method allowed_methods rd = Wm.continue [`GET] rd

  method private to_text rd = 
    let text = Log.debug (fun m -> m "Have been pinged."); "i am alive." in 
    Wm.continue (`String (Printf.sprintf "%s" text)) rd
end

let get_path_info_exn rd wildcard =
  match Wm.Rd.lookup_path_info wildcard rd with 
  | Some p -> p
  | None   -> raise (Path_info_exn wildcard) 

let pull_out_strings l = 
  match l with
  | `List j -> 
      List.map j 
        ~f:(begin function
            | `String s -> s
            | _         -> raise Malformed_data 
            end)
  | _ -> raise Malformed_data

let get_file_list plaintext =
  Cstruct.to_string plaintext
  |> Yojson.Basic.from_string 
  |> pull_out_strings

let make_file_list lst =
  `List (Core.Std.List.map lst ~f:(fun s -> `String s))

let get_permission_list plaintext = 
  Cstruct.to_string plaintext
  |> Yojson.Basic.from_string
  |> begin function
     | `Assoc j -> 
         List.map j 
         ~f:(begin function
         | (permission, `String path) -> (permission, path)
         | _                          -> raise Malformed_data 
         end)
     | _ -> raise Malformed_data
     end

let get_file_content_list plaintext = 
  Cstruct.to_string plaintext
  |> Yojson.Basic.from_string
  |> begin function
     | `Assoc j -> `Assoc j
     | _ -> raise Malformed_data
     end

let get_file_and_capability_list plaintext =
  let plaintext' = Cstruct.to_string plaintext in
  let json = Yojson.Basic.from_string plaintext' in 
  let files = Yojson.Basic.Util.member "files" json |> pull_out_strings in
  let capabilities = Yojson.Basic.Util.member "capabilities" json |> Auth.deserialise_request_capabilities in
  files,capabilities

let decrypt_message_from_peer peer ciphertext iv s =
  let ks,message = CS.decrypt ~ks:(s#get_keying_service) ~peer ~ciphertext ~iv
  in s#set_keying_service ks; message 

let encrypt_message_to_peer peer plaintext s =
  CS.encrypt ~ks:(s#get_keying_service) ~peer ~plaintext
  >|= fun (ks,ciphertext,iv) -> 
    s#set_keying_service ks; 
    Coding.encode_peer_message ~peer:(s#get_address) ~ciphertext ~iv

let attach_required_capabilities target service files s =
  let requests = Core.Std.List.map files ~f:(fun c -> (Auth.CS.token_of_string "R"),(Printf.sprintf "%s/%s/%s" (Peer.host target) service c)) in
  let caps     = Auth.find_permissions s#get_capability_service requests in
  let caps'    = Auth.serialise_request_capabilities caps in 
  `Assoc [
    ("files"       , (make_file_list files));
    ("capabilities", caps');
  ] |> Yojson.Basic.to_string

let read_from_cache peer service files s =
  Silo.read ~client:s#get_silo_client ~peer ~service ~files
  >|= begin function 
      | `Assoc l -> Core.Std.List.partition_tf l ~f:(fun (n,j) -> not(j = `Null))
      | _        -> raise Malformed_data
      end 
  >|= fun (cached,not_cached) -> (cached, (Core.Std.List.map not_cached ~f:(fun (n,j) -> n)))

let write_to_cache peer service file_content s =
  Silo.write ~client:s#get_silo_client ~peer ~service ~contents:(`Assoc file_content)

exception Send_failing_on_retry

let rec send_retry target path body is_retry s =
  encrypt_message_to_peer target (Cstruct.of_string body) s
  >>= fun body' -> Http_client.post ~peer:target ~path ~body:body'
  >>= fun (c,b) -> 
    if (c >= 200 && c < 300) 
    then Lwt.return (c,b) 
    else if not(is_retry) then
      (s#set_keying_service (KS.invalidate s#get_keying_service target);
      send_retry target path body true s)
    else raise Send_failing_on_retry

module Client = struct
  let decrypt_message_from_client ciphertext iv s =
    CS.decrypt' ~key:(s#get_secret_key) ~ciphertext ~iv

  let encrypt_message_to_client message s =
    Cstruct.of_string message
    |> (fun plaintext       -> CS.encrypt' ~key:(s#get_secret_key) ~plaintext)
    |> (fun (ciphertext,iv) -> Coding.encode_client_message ~ciphertext ~iv) 

  class get_local s = object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    val mutable service : string option = None

    val mutable files : string list = []

    method content_types_provided rd = 
      Wm.continue [("text/plain", self#to_text)] rd

    method content_types_accepted rd = Wm.continue [] rd
  
    method allowed_methods rd = Wm.continue [`POST] rd

    method malformed_request rd =
      try 
        match Wm.Rd.lookup_path_info "service" rd with
        | None          -> Wm.continue true rd
        | Some service' -> 
        Cohttp_lwt_body.to_string rd.Wm.Rd.req_body
        >|= (fun message -> Coding.decode_client_message ~message)
        >>= (fun (ciphertext,iv) ->
          let plaintext = decrypt_message_from_client ciphertext iv s in
          let files' = get_file_list plaintext in
          service <- Some service';
          files <- files';
          Wm.continue false rd)
      with
      | Coding.Decoding_failed e -> Wm.continue true rd 
      | Cryptography.CS.Decryption_failed -> Wm.continue true rd

    method process_post rd =
      try
        match service with
        | None          -> Wm.continue false rd
        | Some service' -> 
        Silo.read ~client:s#get_silo_client ~peer:s#get_address ~service:service' ~files
        >|= begin function
            | `Assoc _ as j -> 
                let message = Yojson.Basic.to_string j 
                in encrypt_message_to_client message s
            | _ -> raise Malformed_data
            end
        >>= fun response -> 
          Wm.continue true {rd with resp_body = (Cohttp_lwt_body.of_string response)}
      with 
      | _  -> Wm.continue false rd

    method private to_text rd = 
      Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body
      >>= fun s -> Wm.continue (`String s) rd
  end

  class get_remote s = object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    val mutable target : Peer.t option = None

    val mutable service : string option = None

    val mutable plaintext : Cstruct.t option = None

    method content_types_provided rd = 
      Wm.continue [("text/plain", self#to_text)] rd

    method content_types_accepted rd = Wm.continue [] rd
  
    method allowed_methods rd = Wm.continue [`POST] rd

    method malformed_request rd =
      try 
        match Wm.Rd.lookup_path_info "peer" rd with
        | None       -> Wm.continue true rd
        | Some peer' -> let peer = Peer.create peer' in
        match Wm.Rd.lookup_path_info "service" rd with
        | None          -> Wm.continue true rd
        | Some service' -> 
        Cohttp_lwt_body.to_string rd.Wm.Rd.req_body
        >|= (fun message -> Coding.decode_client_message ~message)
        >>= (fun (ciphertext,iv) ->
          let plaintext' = decrypt_message_from_client ciphertext iv s in
          target <- Some peer;
          service <- Some service';
          plaintext <- Some plaintext';
          Wm.continue false rd)
      with
      | Coding.Decoding_failed e -> Wm.continue true rd 
      | Cryptography.CS.Decryption_failed -> Wm.continue true rd

    method process_post rd =
      try
        match target with
        | None       -> Wm.continue false rd
        | Some peer' -> 
        match service with
        | None          -> Wm.continue false rd
        | Some service' -> 
        match plaintext with
        | None            -> Wm.continue false rd
        | Some plaintext' -> 
        let files = get_file_list plaintext' in
        read_from_cache peer' service' files s (* Note, if a file is just `Null it is assumed to be not cached *)
        >>= fun (cached,to_fetch) ->
          let body = attach_required_capabilities peer' service' to_fetch s in
          send_retry peer' (Printf.sprintf "/peer/get/%s" service') body false s
        >>= (fun (c,b) ->
          let _,ciphertext,iv = Coding.decode_peer_message b in
          let plaintext = decrypt_message_from_peer peer' ciphertext iv s in
          let `Assoc fetched = get_file_content_list plaintext in
          let results = Core.Std.List.append fetched cached in
          let results' = (`Assoc results) |> Yojson.Basic.to_string in
          write_to_cache peer' service' fetched s
          >|= fun () -> encrypt_message_to_client results' s)
        >>= fun response -> 
          Wm.continue true {rd with resp_body = Cohttp_lwt_body.of_string response}
      with
      | _ -> Wm.continue false rd

    method private to_text rd = 
      Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body
      >>= fun s -> Wm.continue (`String s) rd
  end

  class permit s = object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    val mutable service : string option = None

    val mutable target : Peer.t option = None

    val mutable permission_list : (string * string) list = []

    method content_types_provided rd = 
      Wm.continue [("text/plain", self#to_text)] rd

    method content_types_accepted rd = Wm.continue [] rd
  
    method allowed_methods rd = Wm.continue [`POST] rd      

    method malformed_request rd =
      try
        match Wm.Rd.lookup_path_info "peer" rd with
        | None       -> Wm.continue true rd
        | Some peer' -> let peer = Peer.create peer' in
        match Wm.Rd.lookup_path_info "service" rd with
        | None          -> Wm.continue true rd
        | Some service' -> 
        Cohttp_lwt_body.to_string rd.Wm.Rd.req_body
        >|= (fun message -> Coding.decode_client_message ~message)
        >>= (fun (ciphertext,iv) -> 
          let plaintext    = decrypt_message_from_client ciphertext iv s  in
          service <- Some service';
          target <- Some (Peer.create peer');
          permission_list <- get_permission_list plaintext; 
          Wm.continue false rd)
      with 
      | Coding.Decoding_failed e -> 
          Wm.continue true rd
      | Cryptography.CS.Decryption_failed ->
          Wm.continue true rd
      | Malformed_data ->
          Wm.continue true rd

    method process_post rd =
      try
        match target with
        | None -> Wm.continue false rd
        | Some target' ->
        match service with
        | None -> Wm.continue false rd
        | Some service' ->
        let capabilities = Auth.mint s service' permission_list in 
        let p_body       = Auth.serialise_presented_capabilities capabilities in
        let path         = 
          (Printf.sprintf "/peer/permit/%s/%s" 
          (s#get_address |> Peer.host) service') in
        send_retry target' path p_body false s
        >>= fun (c,b) -> 
          (Log.debug (fun m -> m "Server responded to presented capabilities with %d" c); 
          Wm.continue true rd)
      with
      | Send_failing_on_retry -> Wm.continue false rd

    method private to_text rd = 
      Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body
      >>= fun s -> Wm.continue (`String s) rd
  end

  class set_local s = object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    val mutable file_content_to_set : Yojson.Basic.json = `Null

    val mutable service : string option = None

    method content_types_provided rd =
      Wm.continue [("text/plain", self#to_text)] rd

    method content_types_accepted rd = Wm.continue [] rd
  
    method allowed_methods rd = Wm.continue [`POST] rd

    method malformed_request rd =
      try
        match Wm.Rd.lookup_path_info "service" rd with
        | None       -> Wm.continue true rd
        | Some service' -> 
        Cohttp_lwt_body.to_string rd.Wm.Rd.req_body
        >|= (fun message -> Coding.decode_client_message ~message)
        >>= (fun (ciphertext,iv) -> 
          let plaintext = decrypt_message_from_client ciphertext iv s in
            service <- Some service';
            file_content_to_set <- get_file_content_list plaintext;
            Wm.continue false rd)
      with
      | Coding.Decoding_failed e -> 
          Wm.continue true rd
      | Cryptography.CS.Decryption_failed ->
          Wm.continue true rd
      | Malformed_data ->
          Wm.continue true rd

    method process_post rd =
      try
        match service with
        | None -> raise Malformed_data
        | Some service' ->
        match file_content_to_set with
        | `Assoc j as contents ->
            (Silo.write ~client:s#get_silo_client ~peer:s#get_address ~service:service' ~contents
            >>= fun () -> Wm.continue true {rd with resp_body = (Cohttp_lwt_body.of_string "Successfully written.");})
        | _ -> raise Malformed_data
      with
      | Malformed_data -> Wm.continue false rd

    method private to_text rd = 
      Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body
      >>= fun s -> Wm.continue (`String s) rd
  end

  class del_local s = object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    val mutable service : string option = None

    val mutable files : string list = []

    method content_types_provided rd = 
      Wm.continue [("text/plain", self#to_text)] rd

    method content_types_accepted rd = Wm.continue [] rd
  
    method allowed_methods rd = Wm.continue [`POST] rd

    method malformed_request rd =
      try 
        match Wm.Rd.lookup_path_info "service" rd with
        | None          -> Wm.continue true rd
        | Some service' -> 
        Cohttp_lwt_body.to_string rd.Wm.Rd.req_body
        >|= (fun message -> Coding.decode_client_message ~message)
        >>= (fun (ciphertext,iv) ->
          let plaintext = decrypt_message_from_client ciphertext iv s in
          let files' = get_file_list plaintext in
          service <- Some service';
          files <- files';
          Wm.continue false rd)
      with
      | Coding.Decoding_failed e -> Wm.continue true rd 
      | Cryptography.CS.Decryption_failed -> Wm.continue true rd

    method process_post rd =
      try
        match service with
        | None          -> Wm.continue false rd
        | Some service' -> 
        Silo.delete ~client:s#get_silo_client ~peer:s#get_address ~service:service' ~files
        >>= fun () -> 
          Wm.continue true rd
      with 
      | _  -> Wm.continue false rd

    method private to_text rd = 
      Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body
      >>= fun s -> Wm.continue (`String s) rd
  end
end

module Peer = struct 
  class kx_init s = object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    val mutable source : Peer.t option = None

    val mutable public : Cstruct.t option = None

    val mutable group : Nocrypto.Dh.group option = None

    method content_types_provided rd = 
      Wm.continue [("text/json", self#to_json)] rd

    method content_types_accepted rd = Wm.continue [] rd
  
    method allowed_methods rd = Wm.continue [`POST] rd

    method malformed_request rd =
      try
        Cohttp_lwt_body.to_string rd.Wm.Rd.req_body 
        >>= (fun message -> 
          let (source',public',group') = Coding.decode_kx_init ~message
          in source <- Some source'; public <- Some public'; group <- Some group';
          Wm.continue false rd)
      with
      | Coding.Decoding_failed e -> 
          (Log.debug (fun m -> m "Failed to decode message at /peer/kx/init: \n%s" e); 
          Wm.continue true rd)

    method process_post rd =
      match source with
      | None -> Wm.continue false rd
      | Some source' ->
      match public with
      | None -> Wm.continue false rd
      | Some public' ->
      match group with
      | None -> Wm.continue false rd
      | Some group' ->
          let ks,public'' = Cryptography.KS.mediate 
            ~ks:s#get_keying_service 
            ~peer:source' ~group:group' ~public:public' in
          (s#set_keying_service ks);
          let reply = Coding.encode_kx_reply ~peer:(s#get_address) ~public:public'' in
          let r     = reply |> Cohttp_lwt_body.of_string in
          let rd'   = {rd with resp_body=r } in
          Wm.continue true rd'         
  
    method private to_json rd = 
      Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body 
      >>= fun s -> Wm.continue (`String s) rd
  end

  class get s = object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    val mutable service : string option = None

    val mutable files : string list = []

    val mutable source : Peer.t option = None

    method content_types_provided rd = 
      Wm.continue [("text/plain", self#to_text)] rd

    method content_types_accepted rd = Wm.continue [] rd
  
    method allowed_methods rd = Wm.continue [`POST] rd 

    method malformed_request rd = 
      try match Wm.Rd.lookup_path_info "service" rd with
      | None          -> Wm.continue true rd
      | Some service' -> 
          (Cohttp_lwt_body.to_string rd.Wm.Rd.req_body)
          >|= (fun message -> 
            Coding.decode_peer_message ~message)
          >|= (fun (source_peer,ciphertext,iv) ->
              source <- Some source_peer;
              decrypt_message_from_peer source_peer ciphertext iv s)           
          >>= (fun plaintext ->
            let files',capabilities = get_file_and_capability_list plaintext in
            let authorised_files = 
              Auth.authorise files' capabilities 
                (Auth.CS.token_of_string "R")
                s#get_secret_key s#get_address service' in
            (service <- Some service'); (files <- authorised_files); Wm.continue false rd)
      with
      | Coding.Decoding_failed s -> 
          (Log.debug (fun m -> m "Failed to decode message at /peer/get/:service: \n%s" s); 
          Wm.continue true rd)
      | Cryptography.CS.Decryption_failed -> 
          Wm.continue true rd

    method process_post rd =
      try
        match service with 
        | None -> Wm.continue false rd
        | Some service' ->
            Silo.read ~client:s#get_silo_client ~peer:s#get_address ~service:service' ~files:files
            >>= fun j ->
              (match j with 
              | `Assoc _  ->
                  let message = Yojson.Basic.to_string j |> Cstruct.of_string 
                  in (match source with
                  | Some source_peer -> encrypt_message_to_peer source_peer message s
                  | None             -> raise Malformed_data)
              | _ -> raise Malformed_data)
        >>= fun response -> 
          Wm.continue true {rd with resp_body = Cohttp_lwt_body.of_string response}
      with
      | Malformed_data  -> Wm.continue false rd

    method private to_text rd = 
      Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body
      >>= fun st -> Wm.continue (`String st) rd
  end

  class permit s = object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    val mutable capabilities : (Auth.CS.token * Auth.M.t) list = []

    method content_types_provided rd = 
      Wm.continue [("text/plain", self#to_text)] rd

    method content_types_accepted rd = Wm.continue [] rd
  
    method allowed_methods rd = Wm.continue [`POST] rd

    method malformed_request rd =
      try 
        match Wm.Rd.lookup_path_info "peer" rd with
        | None       -> Wm.continue true rd
        | Some peer' -> 
        match Wm.Rd.lookup_path_info "service" rd with
        | None          -> Wm.continue true rd
        | Some service' -> 
        Cohttp_lwt_body.to_string rd.Wm.Rd.req_body
        >|= (fun message -> Coding.decode_peer_message ~message)
        >>= (fun (peer'',ciphertext,iv) -> 
          if not(Peer.create peer' = peer'') then raise Malformed_data
          else 
            let plaintext = 
              decrypt_message_from_peer peer'' ciphertext iv s in
            let capabilities' = 
              Auth.deserialise_presented_capabilities 
              (plaintext |> Cstruct.to_string) in
            (capabilities <- capabilities'; Wm.continue false rd))
      with
      | Coding.Decoding_failed e -> 
          Log.debug (fun m -> m "Failed to decode message at /peer/permit/:peer/:service: \n%s" e);
          Wm.continue true rd
      | Malformed_data -> 
          Wm.continue true rd
      | Cryptography.CS.Decryption_failed -> 
          Wm.continue true rd

    method process_post rd =
      let cs = Auth.record_permissions s#get_capability_service capabilities
      in s#set_capability_service cs; Wm.continue true rd

    method private to_text rd = 
      Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body
      >>= fun st -> Wm.continue (`String st) rd
  end
end  
