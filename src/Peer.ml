type t = {
  host : string ;
  port : int    ;
}

let create h =
  { host = h ; port = 6620 ; } 

let host p = p.host

let port p = p.port

let compare p1 p2 =
  let uri1 = Uri.make ~host:(host p1) ~port:(port p1) () in
  let uri2 = Uri.make ~host:(host p2) ~port:(port p2) () in
  Uri.compare uri1 uri2
