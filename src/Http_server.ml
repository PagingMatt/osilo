open Cohttp_lwt
open Cohttp_lwt_unix
open Cohttp_lwt_unix_io
open Lwt.Infix

let keying_service = ref None
let datakit_client = ref None

class ping = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  method content_types_provided rd = 
    Wm.continue [("text/plain", self#to_text)] rd

  method content_types_accepted rd = Wm.continue [] rd
  
  method allowed_methods rd = Wm.continue [`GET] rd

  method private to_text rd = 
    let text = "pong" in 
    Wm.continue (`String (Printf.sprintf "%s" text)) rd
end

let api = [
  ("/ping/", fun () -> new ping);
] 

let callback _ request body =
  Wm.dispatch' api ~body ~request 
  >|= begin function
      | Some r -> r 
      | None   -> (`Not_found, Cohttp.Header.init (), `String "Not found", [])
      end
  >>= fun (status, headers, body, _) -> Server.respond ~headers ~status ~body ()

let start ~port ~silo ~master =
  let server = Server.make ~callback () in
  let mode   = `TCP (`Port port) in
  keying_service := Some (Cryptography.KS.empty ~capacity:1024 ~master);
  datakit_client := Some (Silo.Client.make ~server:silo); 
  Server.create ~mode server
