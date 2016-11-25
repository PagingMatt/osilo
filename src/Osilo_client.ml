open Core.Std
open Lwt.Infix

let ping host port =
  let peer = Peer.create host port in
  Http_client.get ~peer ~path:"/ping/";
  >|= fun (c,_) -> Printf.printf "Response code: %d" c 

module Terminal = struct
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
      [("ping", ping)]
end

let () = Command.run Terminal.commands
