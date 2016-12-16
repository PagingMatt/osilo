open Core.Std
open Cohttp_lwt
open Cohttp_lwt_unix
open Cohttp_lwt_unix_io
open Lwt.Infix

open Cryptography
open Silo

let src = Logs.Src.create ~doc:"logger for osilo REST server" "osilo.http_server"
module Log = (val Logs.src_log src : Logs.LOG)
  
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
      ("/client/set/local/:service"   , fun () -> new Api.Client.set_local  self);
      ("/client/del/local/:service"   , fun () -> new Api.Client.del_local  self);
      ("/client/permit/:peer/:service", fun () -> new Api.Client.permit     self);
      ("/peer/kx/init/"               , fun () -> new Api.Peer.kx_init      self);
      ("/peer/get/:service"           , fun () -> new Api.Peer.get          self);
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
    Log.info (fun m -> m "Starting osilo REST server for %s on port %d." hostname 6620); 
    Server.create ~mode server
end
