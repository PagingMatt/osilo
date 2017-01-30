open Core.Std
open Cohttp_lwt
open Cohttp_lwt_unix
open Cohttp_lwt_unix_io
open Lwt.Infix

open Cryptography
open Silo

let src = Logs.Src.create ~doc:"logger for osilo REST server" "osilo.http_server"
module Log = (val Logs.src_log src : Logs.LOG)

class type server = object
  method get_address : Peer.t

  method get_keying_service : Cryptography.KS.t

  method set_keying_service : Cryptography.KS.t -> unit

  method get_secret_key : Cstruct.t

  method get_private_key : Nocrypto.Rsa.priv

  method get_capability_service : Auth.CS.t

  method set_capability_service : Auth.CS.t -> unit

  method get_peer_access_log : Peer_access_log.t

  method set_peer_access_log : Peer_access_log.t -> unit

  method get_silo_client : Silo.Client.t

  method start : unit Lwt.t
end
  
class server' hostname key silo = object(self)
  val s_log : unit = Log.info (fun m -> m "Starting peer %s." hostname);

  val address : Peer.t = Peer.create hostname
  method get_address = address 

  val mutable keying_service : KS.t = 
    Log.info (fun m -> m "Creating keying service with empty key cache."); 
    KS.empty ~address:(Peer.create hostname) ~capacity:1024 ~master:key
  method get_keying_service = keying_service
  method set_keying_service k = keying_service <- k
  method get_secret_key = KS.secret keying_service

  val private_key = Nocrypto.Rsa.generate 2048
  method get_private_key = private_key 

  val mutable capability_service : Auth.CS.t = 
    Log.info (fun m -> m "Creating capability service with empty capability tree.");
    Auth.CS.empty
  method get_capability_service = capability_service
  method set_capability_service c = capability_service <- c

  val mutable peer_access_log : Peer_access_log.t = 
    Log.info (fun m -> m "Creating peer access log with empty log tree.");
    Peer_access_log.empty
  method get_peer_access_log = peer_access_log
  method set_peer_access_log p = peer_access_log <- p

  val mutable silo_client : Client.t = 
    Log.info (fun m -> m "Creating data silo client for datakit server at %s." silo);
    Client.create ~server:silo
  method get_silo_client = silo_client

  method private callback _ request body =
    let api = [
      ("/ping/"                       , fun () -> new Api.ping                  );
      ("/client/get/local/:service"   , fun () -> new Api.Client.get_local  self);
      ("/client/get/:peer/:service"   , fun () -> new Api.Client.get_remote self);
      ("/client/set/local/:service"   , fun () -> new Api.Client.set_local  self);
      ("/client/set/:peer/:service"   , fun () -> new Api.Client.set_remote self);
      ("/client/del/local/:service"   , fun () -> new Api.Client.del_local  self);
      ("/client/del/:peer/:service"   , fun () -> new Api.Client.del_remote self);
      ("/client/permit/:peer/:service", fun () -> new Api.Client.permit     self);
      ("/client/inv/:service"         , fun () -> new Api.Client.inv        self);
      ("/peer/kx/init/"               , fun () -> new Api.Peer.kx_init      self);
      ("/peer/rsa/pub"                , fun () -> new Api.Peer.rsa_pub      self);
      ("/peer/get/:service"           , fun () -> new Api.Peer.get          self);
      ("/peer/set/:service"           , fun () -> new Api.Peer.set          self);
      ("/peer/del/:service"           , fun () -> new Api.Peer.del          self);
      ("/peer/inv/:peer/:service"     , fun () -> new Api.Peer.inv          self);
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
    Log.info (fun m -> m "Starting REST server for peer %s on port %d." hostname 6620); 
    Server.create ~mode server
end
