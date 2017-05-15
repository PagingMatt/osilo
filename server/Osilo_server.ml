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
        +> flag "-p" (required string)
        ~doc:"  Port of this server."
        +> flag "-k" (required string)
        ~doc:"  Base 64 secret key."
        +> flag "-d" (required string)
        ~doc:"  Hostname of Datakit server."
        +> flag "-dp" (required string)
        ~doc:"  Port of Datakit server."
        +> flag "-key" (required string)
        ~doc:"  Path to key file."
        +> flag "-cert" (required string)
        ~doc:"  Path to certificate file."
      )
      (fun h p k d dp key cert () -> Lwt_main.run (new Http_server.server' h (int_of_string p) (Coding.decode_cstruct k) d (int_of_string dp) key cert)#start)

  let commands =
    Command.group
      ~summary:"Terminal entry point for osilo server."
      [("start", start)]
end

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Command.run Terminal.commands
