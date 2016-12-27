open Core.Std 

type t = (Peer.t list) File_tree.t 

let empty = File_tree.empty

let build_loc = 
  fun h -> fun s -> fun p -> 
  (fun _ -> String.split (Printf.sprintf "%s/%s/%s" (Peer.host h) s p) ~on:'/')

let build_el =
  fun ps1 -> fun ps2 -> List.append ps1 ps2 |> List.dedup ~compare:Peer.compare

let log l ~host ~peer ~service ~path =
  File_tree.insert ~element:[peer] ~tree:l ~location:(build_loc host service path) ~select:build_el
    ~terminate:(fun _ -> fun _ -> false)

let unlog l ~host ~service ~path =
  File_tree.trim ~tree:l ~location:(build_loc host service path ())

let find l ~host ~service ~path = 
  File_tree.flatten_under ~tree:l ~location:(String.split (Printf.sprintf "%s/%s/%s" (Peer.host host) service path) ~on:'/')
  |> List.fold ~init:[] ~f:List.append 