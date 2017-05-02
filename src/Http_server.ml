open Core.Std
open Cohttp_lwt
open Cohttp_lwt_unix
open Cohttp_lwt_unix_io
open Lwt.Infix

open Cryptography
open Silo

let src = Logs.Src.create ~doc:"logger for osilo REST server" "osilo.http_server"
module Log = (val Logs.src_log src : Logs.LOG)

class type server = object
  method get_address : Peer.t

  method get_secret_key : Cstruct.t

  method get_private_key : Nocrypto.Rsa.priv

  method get_public_key : Nocrypto.Rsa.pub

  method get_keying_service : Cryptography.Keying.t

  method set_keying_service : Cryptography.Keying.t -> unit

  method get_capability_service : Auth.CS.t

  method set_capability_service : Auth.CS.t -> unit

  method get_peer_access_log : Peer_access_log.t

  method set_peer_access_log : Peer_access_log.t -> unit

  method get_silo_client : Silo.Client.t

  method get_data_cache : Data_cache.t

  method set_data_cache : Data_cache.t -> unit

  method start : unit Lwt.t
end

class server' hostname port secret_key silo_address silo_port key cert = object(self)

  val s_log : unit = Log.info (fun m -> m "Starting peer %s." hostname);

  val address : Peer.t = Peer.create hostname port

  method get_address = address

  val secret_key : Cstruct.t = secret_key

  method get_secret_key = secret_key

  val private_key : Nocrypto.Rsa.priv =
    let open Core.Std.Unix in
    let buf  = String.make 65536 'x' in
    let file = Unix.openfile ~mode:[O_RDONLY] (Printf.sprintf "%s" key) in file
    |> Unix.read ~buf
    |> (fun l -> (Unix.close file); String.prefix buf l)
    |> Cstruct.of_string
    |> X509.Encoding.Pem.Private_key.of_pem_cstruct1
    |> begin function
       | `RSA prv -> prv
       end

  method get_private_key = private_key

  method get_public_key = Nocrypto.Rsa.pub_of_priv private_key

  val mutable keying_service : Cryptography.Keying.t =
    Log.info (fun m -> m "Creating keying service with empty public key cache.");
    Keying.empty ~capacity:1048576 (* store 256 4096-bit RSA keys *)
  method get_keying_service = keying_service
  method set_keying_service k = keying_service <- k

  val mutable capability_service : Auth.CS.t =
    Log.info (fun m -> m "Creating capability service with empty capability tree.");
    Auth.CS.empty
  method get_capability_service = capability_service
  method set_capability_service c = capability_service <- c

  val mutable peer_access_log : Peer_access_log.t =
    Log.info (fun m -> m "Creating peer access log with empty log tree.");
    Peer_access_log.empty
  method get_peer_access_log = peer_access_log
  method set_peer_access_log p = peer_access_log <- p

  val mutable silo_client : Client.t =
    Log.info (fun m -> m "Creating data silo client for datakit server at %s:%d." silo_address silo_port);
    Client.create ~server:silo_address ~port:silo_port
  method get_silo_client   = silo_client

  val mutable data_cache : Data_cache.t = Data_cache.create
  method get_data_cache    = data_cache
  method set_data_cache dc = data_cache <- dc

  method private callback _ request body =
    let api = [
      ("/ping/"                       , fun () -> new Api.ping                  );
      ("/client/get/local/:service"   , fun () -> new Api.Client.get_local  self);
      ("/client/get/:peer/:service"   , fun () -> new Api.Client.get_remote self);
      ("/client/set/local/:service"   , fun () -> new Api.Client.set_local  self);
      ("/client/set/:peer/:service"   , fun () -> new Api.Client.set_remote self);
      ("/client/del/local/:service"   , fun () -> new Api.Client.del_local  self);
      ("/client/del/:peer/:service"   , fun () -> new Api.Client.del_remote self);
      ("/client/permit/:peer/:service", fun () -> new Api.Client.permit     self);
      ("/client/inv/:service"         , fun () -> new Api.Client.inv        self);
      ("/peer/pub"                    , fun () -> new Api.Peer.pub          self);
      ("/peer/get/:service"           , fun () -> new Api.Peer.get          self);
      ("/peer/set/:service"           , fun () -> new Api.Peer.set          self);
      ("/peer/del/:service"           , fun () -> new Api.Peer.del          self);
      ("/peer/inv/:peer/:service"     , fun () -> new Api.Peer.inv          self);
      ("/peer/permit/:peer/:service"  , fun () -> new Api.Peer.permit       self);
    ] in
    Wm.dispatch' api ~body ~request
    >>= begin function
        | Some (status, headers, body, _) ->
            Server.respond ~headers ~status ~body ()
        | None ->
            let open Cohttp.Request in
            let r =
              match Cohttp.Header.get_location request.headers with
              | Some l -> Uri.to_string l
              | None   -> ""
            in Server.respond
              ~headers:(Cohttp.Header.init ())
              ~status:`Not_found
              ~body:(`String (Printf.sprintf "Error: API entrypoint at '%s' does not exist." r)) ()
        end

  method start =
    let server = Server.make ~callback:self#callback () in
    let mode   = `TLS (`Crt_file_path cert, `Key_file_path key, `No_password, `Port port) in
    Log.info (fun m -> m "Starting REST server for peer %s on port %d." hostname port);
    Server.create ~mode server
end
