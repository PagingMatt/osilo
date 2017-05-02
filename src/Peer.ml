type t = {
  host : string ;
  port : int    ;
}

let create h p =
  { host = h ; port = p ; }

let host p = p.host

let port p = p.port

let compare p1 p2 =
  let uri1 = Uri.make ~host:(host p1) ~port:(port p1) () in
  let uri2 = Uri.make ~host:(host p2) ~port:(port p2) () in
  Uri.compare uri1 uri2

let t_of_string s =
  match String.split_on_char ',' s with
  | h::p::[] -> create h (int_of_string p)
  | _        -> invalid_arg s

let string_of_t p =
  Printf.sprintf "%s,%d" p.host p.port
