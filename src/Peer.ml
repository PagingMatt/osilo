type t = {
  host : string ;
}

let create h =
  { host = h ; } 

let host p = p.host

let compare p1 p2 =
  let uri1 = Uri.make ~host:(host p1) ~port:6620 () in
  let uri2 = Uri.make ~host:(host p2) ~port:6620 () in
  Uri.compare uri1 uri2
