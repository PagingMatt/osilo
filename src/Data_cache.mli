type t

val create : t

val read :
  peer:string    ->
  service:string ->
  file:string    -> t -> (Yojson.Basic.json * t) option

val write :
  peer:string    ->
  service:string ->
  file:string    ->
  content:Yojson.Basic.json -> t -> t

val invalidate :
  peer:string    ->
  service:string ->
  file:string    -> t -> t
