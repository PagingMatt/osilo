open Core.Std
open Logs

module Terminal = struct 
  let start =
    Command.basic
      ~summary:"Starts an osilo server."
      Command.Spec.(
        empty
        +> flag "-h" (required string)
        ~doc:"  Hostname of this server."
        +> flag "-p" (optional_with_default 8000 int)
        ~doc:"  Port to listen for REST API calls on."
        +> flag "-k" (required string)
        ~doc:"  Base 64 secret key shared with clients"
        +> flag "-ds" (required string)
        ~doc:"  Hostname of Datakit server"
      )
      (fun h p k ds () -> Lwt_main.run (new Http_server.server h p (Coding.decode_cstruct k) ds )#start)

  let commands = 
    Command.group 
      ~summary:"Terminal entry point for osilo server."
      [("start", start)]
end

let () = 
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Command.run Terminal.commands
