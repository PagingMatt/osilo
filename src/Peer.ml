type t = {
	host : string ;
	port : int    ;
}

let host p = p.host

let port p = p.port

let combine_error i j =
	if i=0 || j=0 then i + j
	else i * j 

let compare p1 p2 =
	let hosts = String.compare p1.host p2.host in
	let ports = p1.port - p2.port in
	if (hosts=0) && (ports=0) then 0
	else combine_error hosts ports
