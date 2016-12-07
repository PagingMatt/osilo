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

  val mutable silo_client : Client.t = Client.create ~server:silo
  method get_silo_client = silo_client

  method private callback _ request body =
    let api = [
      ("/ping/"             , fun () -> new Api.ping    self);
      ("/kx/init/"          , fun () -> new Api.kx_init self);
      ("/get/:peer/:service", fun () -> new Api.get     self);
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
    let mode   = `TCP (`Port 6620) in
    Log.info (fun m -> m "Starting osilo REST server for %s on port %d." hostname 6620); 
    Server.create ~mode server
end
