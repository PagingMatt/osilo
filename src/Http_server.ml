open Core.Std
open Cohttp_lwt
open Cohttp_lwt_unix
open Cohttp_lwt_unix_io
open Lwt.Infix

open Cryptography
open Silo

let src = Logs.Src.create ~doc:"logger for osilo REST server" "osilo.http_server"
module Log = (val Logs.src_log src : Logs.LOG)

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
      let (peer,public,group) = Log.info (fun m -> m "A remote peer has initiated a key exchange."); Coding.decode_kx_init ~message in
      let ks,public' = 
        Cryptography.KS.mediate ~ks:s#get_keying_service ~peer ~group ~public in
      (s#set_keying_service ks);
      Log.info (fun m -> m "Mediated key exchange with (%s,%d), passing back public key '%s'" 
        (Peer.host peer) (Peer.port peer) (public' |> Nocrypto.Base64.encode |> Cstruct.to_string));
      let reply = Coding.encode_kx_reply ~peer:(s#get_address) ~public:public' in
      let r     = reply |> Cohttp_lwt_body.of_string in
      let rd'   = {rd with resp_body=r } in
      Wm.continue true rd'         
end

exception No_path
exception No_service of string
exception No_file of string

class get s = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  val mutable data : Yojson.Basic.json = `Null

  method content_types_provided rd = 
    Wm.continue [("text/plain", self#to_text)] rd (* encrypted JSON *)

  method content_types_accepted rd = Wm.continue [] rd
  
  method allowed_methods rd = Wm.continue [`POST] rd

  method private get_path_info_exn rd wildcard =
    match Wm.Rd.lookup_path_info wildcard rd with 
    | Some p -> p
    | None   -> raise No_path 

  method private decrypt_message_from_client body =
    Cohttp_lwt_body.to_string body 
    >|= (fun message -> Coding.decode_message' ~message)
    >|= (fun (c,i)   -> CS.decrypt' ~key:(s#get_secret_key) ~ciphertext:c ~iv:i)

  method private encrypt_message_to_client message =
    Cstruct.of_string message
    |> (fun plaintext       -> CS.encrypt' ~key:(s#get_secret_key) ~plaintext)
    |> (fun (ciphertext,iv) -> Coding.encode_message ~peer:(s#get_address) ~ciphertext ~iv)  

  method process_post rd =
    let plaintext = self#decrypt_message_from_client rd.Wm.Rd.req_body 
    in plaintext 
    >|= Cstruct.to_string
    >|= Yojson.Basic.from_string
    >|= begin function
        | `List j -> 
            List.map j begin function
            | `String s -> s
            | _         -> raise (No_file "File was not string") 
            end
        | _ -> raise (No_file "No JSON list provided")
        end
    >>= fun files ->
      try 
        let peer    = get_path_info_exn rd "peer"    in 
        let service = get_path_info_exn rd "service" in
          (if peer=(Peer.host s#get_address) then
            Silo.read ~client:s#get_silo_client ~peer:s#get_address ~service ~files
          else
            plaintext
            >>= (fun plaintext -> CS.encrypt ~ks:(s#get_keying_service) ~peer:(Peer.create peer 6620) ~plaintext)
            >|= (fun (ks,ciphertext,iv) -> s#set_keying_service ks; Coding.encode_message ~peer:(s#get_address) ~ciphertext ~iv)
            >>= (fun body -> Http_client.post ~peer:(Peer.create peer 6620) ~path:(Printf.sprintf "/get/%s/%s" peer service) ~body) 
            >|= (fun (c,b) -> Yojson.Basic.from_string b))
          >>= fun j -> (data <- j);
              match j with 
              | `Assoc _  ->
                Yojson.Basic.to_string j
                |> self#encrypt_message_to_client
                |> fun s' -> Wm.continue true {rd with resp_body = Cohttp_lwt_body.of_string s'}
              | _         -> Wm.continue false rd  
      with
      | No_path      -> Log.err (fun m -> m "No path"); Wm.continue false rd  
      | No_service s -> Log.err (fun m -> m "No service found in the path '%s'" s); Wm.continue false rd  
      | No_file s    -> Log.err (fun m -> m "No file found in the path '%s'" s); Wm.continue false rd  

  method private to_text rd = 
    Cohttp_lwt_body.to_string rd.Wm.Rd.resp_body
    >>= fun s -> Wm.continue (`String s) rd
end
  
class ping s = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  method content_types_provided rd = 
    Wm.continue [("text/plain", self#to_text)] rd

  method content_types_accepted rd = Wm.continue [] rd
  
  method allowed_methods rd = Wm.continue [`GET] rd

  method private to_text rd = 
    let text = Log.info (fun m -> m "Pinged."); "pong" in 
    Wm.continue (`String (Printf.sprintf "%s" text)) rd
end

class server hostname port key silo = object(self)
  val address : Peer.t = Peer.create hostname port
  method get_address = address 

  val mutable keying_service : KS.t = KS.empty ~address:(Peer.create hostname port) ~capacity:1024 ~master:key
  method get_keying_service = keying_service
  method set_keying_service k = keying_service <- k
  method get_secret_key = KS.secret keying_service

  val mutable silo_client : Client.t = Client.create ~server:silo
  method get_silo_client = silo_client

  method private callback _ request body =
    let api = [
      ("/ping/"             , fun () -> new ping    self);
      ("/kx/init/"          , fun () -> new kx_init self);
      ("/get/:peer/:service", fun () -> new get     self);
      (*("/set/:peer/:service", fun () -> new set     self);*)
    ] in
    Wm.dispatch' api ~body ~request 
    >|= begin function
        | Some r -> r 
        | None   -> (`Not_found, Cohttp.Header.init (), `String "Not found", [])
        end
    >>= fun (status, headers, body, _) -> Server.respond ~headers ~status ~body ()

  method start =
    let server = Server.make ~callback:self#callback () in
    let mode   = `TCP (`Port port) in
    Log.info (fun m -> m "Starting REST server on port %d, hostname is %s" port hostname); 
    Server.create ~mode server
end
