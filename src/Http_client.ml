open Lwt
open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

let build_uri ~peer ~path = 
  Uri.make ~scheme:"http" ~host:(Peer.host peer) ~port:(Peer.port peer) ~path:path ()

let handle_http_resp (r,b) =
  let code = r |> Response.status |> Code.code_of_status in
  Cohttp_lwt_body.to_string b 
  >|= fun body -> (code,body)

let get ~peer ~path =
  Client.get (build_uri ~peer ~path) 
  >>= handle_http_resp

let post ~peer ~path ~body =
  Client.post (build_uri ~peer ~path) ~body:(Cohttp_lwt_body.of_string body) 
  >>= handle_http_resp
