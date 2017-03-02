type t

val read : string -> t -> (Yojson.Basic.json * t) option

val write : string -> Yojson.Basic.json -> t -> t

val invalidate : string -> t -> t 
