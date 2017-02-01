open Lwt
open Lwt.Infix
open Core.Std
open Cohttp
open Cohttp_lwt_unix

let src = Logs.Src.create ~doc:"logger for HTTP client" "osilo.http_client"
module Log = (val Logs.src_log src : Logs.LOG)

let build_uri ~peer ~path = 
  Uri.make ~scheme:"https" ~host:(Peer.host peer) ~port:6620 ~path:path ()

let handle_http_resp (r,b) =
  let code = r |> Response.status |> Code.code_of_status in
  Cohttp_lwt_body.to_string b 
  >|= fun body -> (code,body)

let get ~peer ~path =
  let uri = build_uri ~peer ~path in
  Log.debug (fun m -> m "GET %s" (Uri.to_string uri));
  Client.get uri
  >>= handle_http_resp

let post ~peer ~path ~body =
  let uri = build_uri ~peer ~path in
  Log.debug (fun m -> m "POST %s... to %s" (String.sub body 0 4) (Uri.to_string uri));
  Client.post uri ~body:(Cohttp_lwt_body.of_string body) 
  >>= handle_http_resp
