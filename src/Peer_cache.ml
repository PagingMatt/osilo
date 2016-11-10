(* Although different values will be in different caches, the key type is always the same and 
 * sufficient to identify a peer over a network. Note, currently this uses a string hostname, this
 * should be migrated over to something more type safe - IP vs. URI as they will be regarded as 
 * different hosts *)
module Peer_key : sig 
  type t
  val equal : t -> t -> bool
  val hash  : t -> int 
end = struct  
  (* Type for peer keys contains enough to identify them over a network *)
  type t = {
    host : string ;
    port : int    ;
  }

  (* Keys are equal when they have the same host and port *)
  let equal k1 k2 = (k1.host = k2.host) && (k1.port = k2.port)

  (* Hash the record to get the hash of the key *)
  let hash k = Hashtbl.hash k
end


