open Core.Std
open Lwt.Infix
open Logs

exception Get_failed
exception Kx_failed

let inv server service path key =
  let server' = Peer.create server in
  let message = (`List [`String path]) |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/inv/%s" service in
  Http_client.post ~peer:server' ~path ~body:message ~key ()
  >|= (fun (c,_) -> Printf.printf "%d" c) 

let give server peer service file key token =
  let server' = Peer.create server in
  let message = (`Assoc [(token, `String file)]) |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/permit/%s/%s" peer service in
  Http_client.post ~peer:server' ~path ~body:message ~key ()
  >|= (fun (c,_) -> Printf.printf "%d" c) 

let set_my () = 
  let key = "testtesttesttesttesttesttesttest" in
  let peer = Peer.create "172.16.54.52" in
  let message = (`Assoc [("new-dir/test-file",`String "test value in file")]) |> Yojson.Basic.to_string in
  let path = "/client/set/local/master" in
  Http_client.post ~peer ~path ~body:message ~key ()
  >|= fun _ -> ()

let set_their () = 
  let key = "testtesttesttesttesttesttesttest" in
  let peer = Peer.create "192.168.1.86" in
  let message = (`Assoc [("new-dir/test-file",`String "test value in file")]) |> Yojson.Basic.to_string in
  let path = "/client/set/192.168.1.77/foo" in
  Http_client.post ~peer ~path ~body:message ~key ()
  >|= fun (c,_) -> Printf.printf "%d\n" c

let del_my () = 
  let key  = "testtesttesttesttesttesttesttest" in
  let peer = Peer.create "172.16.54.52" in
  let message = (`List [(`String "new-dir/test-file5")]) |> Yojson.Basic.to_string in
  let path = "/client/del/local/master" in
  Http_client.post ~peer ~path ~body:message ~key ()
  >|= fun _ -> ()

let get_my host port service file key =
  let peer = Peer.create host in
  let message = (`List [`String file]) |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/get/local/%s" service in
  Http_client.post ~peer ~path ~body:message ~key ()
  >|= (fun (c,b) -> Printf.printf "%s\n\n" b) 

let del_their host peer service file key =
  let host' = Peer.create host in
  let message = (`List [`String file]) |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/del/%s/%s" peer service in
  Http_client.post ~peer:host' ~path ~body:message ~key ()
  >|= fun (c,_) -> Printf.printf "%d" c

let get_their host peer service file key =
  let server = Peer.create host in
  let peer' = Peer.create peer in
  let message = (`List [(`Assoc [("path",`String "bar");("check_cache", `Bool false); ("write_back", `Bool false)])]) |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/get/%s/%s" (Peer.host peer') service in
  Http_client.post ~peer:server ~path ~body:message ~key ()
  >|= (fun (c,b) -> Printf.printf "%s\n\n" b) 

let ping host port =
  let peer = Peer.create host in
  Http_client.get ~peer ~path:"/ping/";
  >|= fun (c,_) -> Printf.printf "Response code: %d\n" c 

module Terminal = struct

  let set_my = 
    Command.basic
      ~summary:"Set a piece of my data"
      Command.Spec.(
        empty
      )
      (fun () -> Lwt_main.run (set_my ()))

  let set_their = 
    Command.basic
      ~summary:"Set a piece of their data"
      Command.Spec.(
        empty
      )
      (fun () -> Lwt_main.run (set_their ()))

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

  let del_my = 
    Command.basic
      ~summary:"Delete a piece of my data"
      Command.Spec.(
        empty
      )
      (fun () -> Lwt_main.run (del_my ()))

  let get_their = 
    Command.basic
      ~summary:"Get a piece of their data"
      Command.Spec.(
        empty
        +> flag "-h" (required string) ~doc:" Server to target request at."
        +> flag "-p" (required string   ) ~doc:" Peer to talk to."
        +> flag "-s" (required string) ~doc:" Service data is from."
        +> flag "-f" (required string) ~doc:" Logical file name."
        +> flag "-k" (required string) ~doc:" Base64 string private key."
      )
      (fun h p s f k () -> Lwt_main.run (get_their h p s f k))

  let del_their = 
    Command.basic
      ~summary:"Delete a piece of their data"
      Command.Spec.(
        empty
        +> flag "-h" (required string) ~doc:" Server to target request at."
        +> flag "-p" (required string) ~doc:" Peer to talk to."
        +> flag "-s" (required string) ~doc:" Service data is from."
        +> flag "-f" (required string) ~doc:" Logical file name."
        +> flag "-k" (required string) ~doc:" Base64 string private key."
      )
      (fun h p s f k () -> Lwt_main.run (del_their h p s f k))

  let give =
    Command.basic
      ~summary:"Give a host capabilities to a file on a service in my databox"
      Command.Spec.(
        empty
        +> flag "-h" (required string) ~doc:" My server."
        +> flag "-p" (required string) ~doc:" Peer to give capability to."
        +> flag "-s" (required string) ~doc:" Service data is on."
        +> flag "-f" (required string) ~doc:" Logical file name."
        +> flag "-k" (required string) ~doc:" Base64 string private key."
        +> flag "-t" (required string) ~doc:" Token [ W | R ] to express permission to give"
      )
      (fun h p s f k t () -> Lwt_main.run (give h p s f k t))

  let ping = 
    Command.basic
      ~summary:"Ping specified osilo server."
      Command.Spec.(
        empty
        +> flag "-h" (required string) ~doc:" Host to target request at."
        +> flag "-p" (required int   ) ~doc:" Port to talk to at target."
      )
      (fun h p () -> Lwt_main.run (ping h p))

  let inv = 
    Command.basic
      ~summary:"Invalidate remotely cached data."
      Command.Spec.(
        empty
        +> flag "-h" (required string) ~doc:" Host to target request at."
        +> flag "-s" (required string) ~doc:" Service to inavlidate path on."
        +> flag "-p" (required string) ~doc:" Path to invalidate at and below."
        +> flag "-k" (required string) ~doc:" Key for server."
      )
      (fun h s p k () -> Lwt_main.run (inv h s p k))

  let commands = 
    Command.group 
      ~summary:"Terminal entry point for osilo terminal client."
      [("inv",inv);("del-my",del_my);("del-their",del_their);("get-my",get_my);("get-their",get_their);("set-my",set_my);("set-their",set_their);("ping", ping);("give",give)]
end

let () = 
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Nocrypto_entropy_unix.initialize ();
  Command.run Terminal.commands
