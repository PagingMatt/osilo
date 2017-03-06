open Base
open Logs
open Cmdliner

let hostname =
  let doc = "Hostname of the machine running this Osilo peer." in
  Arg.(required & pos 0 (some string) None & info ["h"; "hostname"] ~docv:"HOSTNAME" ~doc)

let datakit_server =
  let doc = "Address of the 9P endpoint exposed by this peer's Datakit server on port 5640." in
  Arg.(required & pos 0 (some string) None & info ["d"; "datakit"] ~docv:"DATAKIT" ~doc)

let secret_key =
  let doc = "Base64 secret key for symmetric encryption in the data silo." in
  Arg.(required & pos 0 (some string) None & info ["s"; "secret"] ~docv:"SECRET" ~doc)

let private_key =
  let doc = "Location of the PEM file containing this peer's RSA private key." in
  Arg.(required & pos 0 (some string) None & info ["p"; "private"] ~docv:"PRIVATE" ~doc)

let certificate =
  let doc = "Location of the PEM file containing this peer's certificate." in
  Arg.(required & pos 0 (some string) None & info ["c"; "certificate"] ~docv:"CERTIFICATE" ~doc)

let start h d k key cert =
  Lwt_main.run (new Http_server.server' h (Coding.decode_cstruct k) d key cert)#start

module Terminal = struct
  let start_t,start_info =
    let doc = "start an osilo peer" in
    let man = [] in
    Term.(const start $ hostname $ datakit_server $ secret_key $ private_key $ certificate),
    Term.info "osilo" ~version:"v0.1.0" ~doc ~man
end

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Term.eval (Terminal.start_t, Terminal.start_info)
  |> fun _ -> ()
