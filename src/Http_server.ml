open Cohttp_lwt
open Cohttp_lwt_unix
open Cohttp_lwt_unix_io

open Lwt.Infix

class ping = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  method content_types_provided rd = Wm.continue [("text/plain", self#to_text)] rd
  method content_types_accepted rd = Wm.continue [] rd
  
  method allowed_methods rd = Wm.continue [`GET] rd

  method private to_text rd = 
    let text = "pong"
    in Wm.continue (`String (Printf.sprintf "%s" text)) rd
end

let api = [
  ("/ping/", fun () -> new ping);
] 

let callback _ request body =
  Wm.dispatch' api ~body ~request 
  >|= begin function
        | None -> (`Not_found, Cohttp.Header.init (), `String "Not found", [])
        | Some result -> result 
      end
  >>= fun (status, headers, body, _) 
      -> Server.respond ~headers ~status ~body ()

let start ~port =
  let server = Server.make ~callback () in
  let mode   = `TCP (`Port port) in
  Server.create ~mode server
