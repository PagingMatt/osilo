open Core.Std

module Terminal = struct 
  let start =
    Command.basic
      ~summary:"Starts an osilo server."
      Command.Spec.(
        empty
        +> flag "-p" (optional_with_default 8000 int)
        ~doc:"  Port to listen for REST API calls on."
      )
      (fun p () -> Lwt_main.run (Http_server.start p))

  let commands = 
    Command.group 
      ~summary:"Terminal entry point for osilo server."
      [("start", start)]
end

let () = Command.run Terminal.commands
