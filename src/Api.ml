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

let get_file_list plaintext =
  Cstruct.to_string plaintext
  |> Yojson.Basic.from_string 
  |> begin function
     | `List j -> 
         List.map j begin function
         | `String s -> s
         | _         -> raise Malformed_data 
         end
     | _ -> raise Malformed_data
     end

let decrypt_message_from_peer peer ciphertext iv s =
  let ks,message = CS.decrypt ~ks:(s#get_keying_service) ~peer ~ciphertext ~iv
  in s#set_keying_service ks; message 

let encrypt_message_to_peer peer plaintext s =
  CS.encrypt ~ks:(s#get_keying_service) ~peer ~plaintext
  >|= fun (ks,ciphertext,iv) -> 
    s#set_keying_service ks; 
    Coding.encode_peer_message ~peer:(s#get_address) ~ciphertext ~iv

module Client = struct
  let decrypt_message_from_client ciphertext iv s =
    CS.decrypt' ~key:(s#get_secret_key) ~ciphertext ~iv

  let encrypt_message_to_client message s =
    Cstruct.of_string message
    |> (fun plaintext       -> CS.encrypt' ~key:(s#get_secret_key) ~plaintext)
    |> (fun (ciphertext,iv) -> Coding.encode_client_message ~ciphertext ~iv) 

  class get_local s = object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    val mutable data : Yojson.Basic.json = `Null

    method content_types_provided rd = 
      Wm.continue [("text/plain", self#to_text)] rd

    method content_types_accepted rd = Wm.continue [] rd
  
    method allowed_methods rd = Wm.continue [`POST] rd

    method private client_get_my_data service ciphertext iv =
      let plaintext = decrypt_message_from_client ciphertext iv s in
      let files     = get_file_list plaintext           in
      Silo.read ~client:s#get_silo_client ~peer:s#get_address ~service ~files
      >|= fun j -> 
        (data <- j);
        match j with 
        | `Assoc _  -> 
            let message = Yojson.Basic.to_string j 
            in encrypt_message_to_client message s
        | _         -> raise Malformed_data

    method process_post rd =
      try
        let service     = get_path_info_exn rd "service" in
        Cohttp_lwt_body.to_string rd.Wm.Rd.req_body
        >|= (fun message -> Coding.decode_client_message ~message)
        >>= (fun (ciphertext,iv) -> 
            self#client_get_my_data service ciphertext iv)
        >>= fun response -> 
          Wm.continue true {rd with resp_body = Cohttp_lwt_body.of_string response}
      with
      | Path_info_exn w -> Wm.continue false rd  
      | Malformed_data  -> Wm.continue false rd
      | Fetch_failed t  -> Wm.continue false rd

    method private to_text rd = 
      Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body
      >>= fun s -> Wm.continue (`String s) rd
  end

  class get_remote s = object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    val mutable data : Yojson.Basic.json = `Null

    method content_types_provided rd = 
      Wm.continue [("text/plain", self#to_text)] rd

    method content_types_accepted rd = Wm.continue [] rd
  
    method allowed_methods rd = Wm.continue [`POST] rd

    method private client_get_peer_data target service ciphertext iv =   
      let plaintext = decrypt_message_from_client ciphertext iv s in
      encrypt_message_to_peer target plaintext s
      >>= (fun body -> 
        Http_client.post 
          ~peer:target 
          ~path:(Printf.sprintf "/peer/get/%s" service) 
          ~body) 
      >|= (fun (c,b) -> 
        if c=200 then 
          let _,ciphertext,iv = Coding.decode_peer_message b in
          let plaintext = decrypt_message_from_peer target ciphertext iv s in
          encrypt_message_to_client (Cstruct.to_string plaintext) s
        else
          raise (Fetch_failed target))

    method process_post rd =
      try
        let target      = Peer.create (get_path_info_exn rd "peer") in
        let service     = get_path_info_exn rd "service"            in
        Cohttp_lwt_body.to_string rd.Wm.Rd.req_body
        >|= (fun message -> Coding.decode_client_message ~message)
        >>= (fun (ciphertext,iv) -> 
            self#client_get_peer_data target service ciphertext iv)
        >>= fun response -> 
          Wm.continue true {rd with resp_body = Cohttp_lwt_body.of_string response}
      with
      | Path_info_exn w -> Wm.continue false rd  
      | Malformed_data  -> Wm.continue false rd
      | Fetch_failed t  -> Wm.continue false rd

    method private to_text rd = 
      Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body
      >>= fun s -> Wm.continue (`String s) rd
  end
end

module Peer = struct 
  class kx_init s = object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    val mutable public_key : Cstruct.t option = None

    method content_types_provided rd = 
      Wm.continue [("text/json", self#to_json)] rd

    method content_types_accepted rd = Wm.continue [] rd
  
    method allowed_methods rd = Wm.continue [`POST] rd

    method private to_json rd = 
      Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body 
      >>= fun s -> Wm.continue (`String s) rd

    method process_post rd =
      Cohttp_lwt_body.to_string rd.Wm.Rd.req_body 
      >>= fun message -> 
        let (peer,public,group) = Coding.decode_kx_init ~message in
        let ks,public' = 
          Cryptography.KS.mediate 
            ~ks:s#get_keying_service 
            ~peer ~group ~public in
        (s#set_keying_service ks);
        let reply = Coding.encode_kx_reply ~peer:(s#get_address) ~public:public' in
        let r     = reply |> Cohttp_lwt_body.of_string in
        let rd'   = {rd with resp_body=r } in
        Wm.continue true rd'         
  end

  class get s = object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    val mutable data : Yojson.Basic.json = `Null

    method content_types_provided rd = 
      Wm.continue [("text/plain", self#to_text)] rd

    method content_types_accepted rd = Wm.continue [] rd
  
    method allowed_methods rd = Wm.continue [`POST] rd 

    method private peer_get_my_data service source ciphertext iv =
      let plaintext = decrypt_message_from_peer source ciphertext iv s in
      let files = get_file_list plaintext in
      Silo.read ~client:s#get_silo_client ~peer:s#get_address ~service ~files
      >>= fun j -> 
        (data <- j);
        match j with 
        | `Assoc _  -> 
            let message = Yojson.Basic.to_string j |> Cstruct.of_string 
            in encrypt_message_to_peer source message s
        | _         -> raise Malformed_data

    method process_post rd =
      try
        let service     = get_path_info_exn rd "service" in
        Cohttp_lwt_body.to_string rd.Wm.Rd.req_body
        >|= (fun message -> Coding.decode_peer_message ~message)
        >>= (fun (source_peer,ciphertext,iv) -> 
          self#peer_get_my_data service source_peer ciphertext iv)
        >>= fun response -> 
          Wm.continue true {rd with resp_body = Cohttp_lwt_body.of_string response}
        with
        | Path_info_exn w -> Wm.continue false rd  
        | Malformed_data  -> Wm.continue false rd
        | Fetch_failed t  -> Wm.continue false rd

    method private to_text rd = 
      Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body
      >>= fun s -> Wm.continue (`String s) rd
  end
end  
