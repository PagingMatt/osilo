open Core.Std
open Cohttp_lwt
open Cohttp_lwt_unix
open Cohttp_lwt_unix_io
open Lwt.Infix

open Cryptography
open Silo

let src = Logs.Src.create ~doc:"logger for osilo REST server" "osilo.http_server"
module Log = (val Logs.src_log src : Logs.LOG)

class set s = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  method content_types_provided rd =

    Wm.continue [("text/plain", self#to_text)] rd

  method content_types_accepted rd = Wm.continue [] rd
  method allowed_methods rd = Wm.continue [`POST] rd
  
  method private get_file_content_list plaintext =
    Cstruct.to_string plaintext
    |> Yojson.Basic.from_string 

  method private get_path_info_exn rd wildcard =
    match Wm.Rd.lookup_path_info wildcard rd with 
    | Some p -> p
    | None   -> raise (Path_info_exn wildcard) 

  method private encrypt_message_to_client message =
    Cstruct.of_string message
    |> (fun plaintext       -> CS.encrypt' ~key:(s#get_secret_key) ~plaintext)
    |> (fun (ciphertext,iv) -> Coding.encode_message ~peer:(s#get_address) ~ciphertext ~iv)  
  method private decrypt_message_from_client ciphertext iv =

    CS.decrypt' ~key:(s#get_secret_key) ~ciphertext ~iv
  method private client_set_my_data service ciphertext iv =

    let plaintext = self#decrypt_message_from_client ciphertext iv  in
    let contents  = self#get_file_content_list plaintext            in
    Silo.write ~client:s#get_silo_client ~peer:s#get_address ~service ~contents
  method process_post rd =

    try
      Log.debug (fun m -> m "A write request for some data has been received."); 
      let target_peer = Peer.create (self#get_path_info_exn rd "peer") in 
      let service     = self#get_path_info_exn rd "service" in
      Cohttp_lwt_body.to_string rd.Wm.Rd.req_body
      Log.debug (fun m -> m "The write request is for data for %s on %s." service (Peer.host target_peer));
      >|= (fun message -> Coding.decode_message ~message)
      >>= (fun (source_peer,ciphertext,iv) -> 
        Log.debug (fun m -> m "The write request originated from someone claiming to be %s and has ciphertext '%s' and initial vector '%s'."
        if target_peer = s#get_address then
          (Peer.host source_peer) (Cstruct.to_string ciphertext) (Cstruct.to_string iv));
          (Log.debug (fun m -> m "The write request is to be performed on my repository.");
            (Log.debug (fun m -> m "The write request is from someone claiming to be my client.");
          if source_peer = s#get_address then
            self#client_set_my_data service ciphertext iv
          else
            >>= fun () -> Wm.continue true {rd with resp_body = (Cohttp_lwt_body.of_string "Successfully written.");})
            (Log.debug (fun m -> m "The write request is not from my client.");
            raise Cannot_set))
        else
          raise Cannot_set)
    | _ -> Wm.continue false rd
    with

    Log.debug (fun m -> m "Sending response for write request.");
  method private to_text rd = 
    Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body
    >>= fun s -> Wm.continue (`String s) rd
end
  
class server hostname key silo = object(self)
  val address : Peer.t = Peer.create hostname
  method get_address = address 

  val mutable keying_service : KS.t = KS.empty ~address:(Peer.create hostname) ~capacity:1024 ~master:key
  method get_keying_service = keying_service
  method set_keying_service k = keying_service <- k
  method get_secret_key = KS.secret keying_service

  val mutable capability_service : Auth.CS.t = Auth.CS.create
  method get_capability_service = capability_service
  method set_capability_service c = capability_service <- c

  val mutable silo_client : Client.t = Client.create ~server:silo
  method get_silo_client = silo_client

  method private callback _ request body =
    let api = [
      ("/ping/"                       , fun () -> new Api.ping              self);
      ("/client/get/local/:service"   , fun () -> new Api.Client.get_local  self);
      ("/client/get/:peer/:service"   , fun () -> new Api.Client.get_remote self);
      ("/client/permit/:peer/:service", fun () -> new Api.Client.permit     self);
      ("/peer/kx/init/"               , fun () -> new Api.Peer.kx_init      self);
      ("/peer/get/:service"           , fun () -> new Api.Peer.get          self);
      ("/peer/permit/:peer/:service"  , fun () -> new Api.Peer.permit       self);
    ] in
    Wm.dispatch' api ~body ~request 
    >|= begin function
        | Some r -> r 
        | None   -> (`Not_found, Cohttp.Header.init (), `String "Not found", [])
        end
    >>= fun (status, headers, body, _) -> Server.respond ~headers ~status ~body ()

  method start =
    let server = Server.make ~callback:self#callback () in
    let mode   = `TCP (`Port 6620) in
    Log.info (fun m -> m "Starting osilo REST server for %s on port %d." hostname 6620); 
    Server.create ~mode server
end
