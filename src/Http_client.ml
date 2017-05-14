open Lwt
open Lwt.Infix
open Core.Std
open Cohttp
open Cohttp_lwt_unix

let src = Logs.Src.create ~doc:"logger for HTTP client" "osilo.http_client"
module Log = (val Logs.src_log src : Logs.LOG)

let build_uri ~peer ~path =
  Uri.make ~scheme:"https" ~host:(Peer.host peer) ~port:(Peer.port peer) ~path:path ()

let handle_http_resp (r,b) =
  Cohttp_lwt_body.to_string b >|= fun b' ->
  (Response.status r |> Code.code_of_status, b')

type auth =
  | None
  | Key of string
  | Sig of string * string

let get ~peer ~path =
  let uri = build_uri ~peer ~path in
  Client.get uri
  >>= handle_http_resp

let post ~peer ~path ~body ~auth =
  let uri = build_uri ~peer ~path in
  let headers =
    match auth with
    | None      -> (Header.init ())
    | Key k     -> (Header.add_authorization (Header.init ()) (`Other k))
    | Sig (p,s) -> (Header.add_authorization (Header.init ()) (`Basic (p,s)))
  in
  Client.post uri ~body:(Cohttp_lwt_body.of_string body) ~headers
  >>= handle_http_resp
