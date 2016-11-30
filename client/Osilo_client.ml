open Core.Std
open Lwt.Infix
open Logs

exception Get_failed
exception Kx_failed

let get_my host port service file key =
  let key  = 
    match key |> Cstruct.of_string |> Nocrypto.Base64.decode with
    | Some c -> c
    | None   -> raise Get_failed
  in
  let peer = Peer.create host port in
  let plaintext = (`List [`String file]) |> Yojson.Basic.to_string |> Cstruct.of_string in
  let c,i = Cryptography.CS.encrypt' ~key ~plaintext in
  let body = Coding.encode_message' ~ciphertext:c ~iv:i in
  let path = Printf.sprintf "/get/my/%s" service in
  Printf.printf "%s" body; Http_client.post ~peer ~path ~body
  >|= (fun (c,b) -> Coding.decode_message b) 
  >|= (fun (p,ciphertext,iv) -> Cryptography.CS.decrypt' ~key ~ciphertext ~iv)
  >|= Cstruct.to_string
  >|= (fun m -> Printf.printf "%s\n\n" m) 

let kx_test host port =
  let group = Nocrypto.Dh.gen_group 32 in
  let s,p   = Nocrypto.Dh.gen_key group in
  let me    = Peer.create "127.0.0.1" 8001 in
  let peer  = Peer.create host port in
  let msg   = Coding.encode_kx_init me p group in
  Http_client.post ~peer ~path:"/kx/init/" ~body:msg
  >|= fun (c,b) ->
    let peer',public = Coding.decode_kx_reply b in
        let shared = 
          match Nocrypto.Dh.shared group s public with 
          | Some s -> s
          | None   -> raise Kx_failed
        in
          Printf.printf "%s" (shared |> Nocrypto.Base64.encode |> Cstruct.to_string)

let ping host port =
  let peer = Peer.create host port in
  Http_client.get ~peer ~path:"/ping/";
  >|= fun (c,_) -> Printf.printf "Response code: %d\n" c 

module Terminal = struct
  let kx_test = 
    Command.basic
      ~summary:"Test KX with osilo server."
      Command.Spec.(
        empty
        +> flag "-h" (required string) ~doc:" Host to target request at."
        +> flag "-p" (required int   ) ~doc:" Port to talk to at target."
      )
      (fun h p () -> Lwt_main.run (kx_test h p))

  let get_my = 
    Command.basic
      ~summary:"Get a piece of my data"
      Command.Spec.(
        empty
        +> flag "-h" (required string) ~doc:" Host to target request at."
        +> flag "-p" (required int   ) ~doc:" Port to talk to at target."
        +> flag "-s" (required string) ~doc:" Service data is from."
        +> flag "-f" (required string) ~doc:" Logical file name."
        +> flag "-k" (required string) ~doc:" Base64 string private key."
      )
      (fun h p s f k () -> Lwt_main.run (get_my h p s f k))


  let ping = 
    Command.basic
      ~summary:"Ping specified osilo server."
      Command.Spec.(
        empty
        +> flag "-h" (required string) ~doc:" Host to target request at."
        +> flag "-p" (required int   ) ~doc:" Port to talk to at target."
      )
      (fun h p () -> Lwt_main.run (ping h p))

  let commands = 
    Command.group 
      ~summary:"Terminal entry point for osilo terminal client."
      [("get-my",get_my);("kx",kx_test);("ping", ping)]
end

let () = 
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Nocrypto_entropy_unix.initialize ();
  Command.run Terminal.commands
