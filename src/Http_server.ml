open Cohttp_lwt
open Cohttp_lwt_unix
open Cohttp_lwt_unix_io
open Lwt.Infix

open Cryptography
open Silo
  
class ping s = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  method content_types_provided rd = 
    Wm.continue [("text/plain", self#to_text)] rd

  method content_types_accepted rd = Wm.continue [] rd
  
  method allowed_methods rd = Wm.continue [`GET] rd

  method private to_text rd = 
    let text = "pong" in 
    Wm.continue (`String (Printf.sprintf "%s" text)) rd
end

class server hostname port key silo_host = object(self)
  val address : Peer.t = Peer.create hostname port
  
  val mutable keying_service : KS.t = KS.empty ~capacity:1024 ~master:key

  val mutable silo_client : Client.t = Client.make ~server:(Uri.make ~host:silo_host ())

  method private callback _ request body =
    let api = [
      ("/ping/", fun () -> new ping self);
    ] in
    Wm.dispatch' api ~body ~request 
    >|= begin function
        | Some r -> r 
        | None   -> (`Not_found, Cohttp.Header.init (), `String "Not found", [])
        end
    >>= fun (status, headers, body, _) -> Server.respond ~headers ~status ~body ()

  method start ()  =
    let server = Server.make ~callback:self#callback () in
    let mode   = `TCP (`Port port) in
    Server.create ~mode server
end
