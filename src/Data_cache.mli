(** In-memory L1 cache to optimise reading over 9P link to silo. *)

type t
(** Internal type of the data cache. *)

val create : t
(** Value to create new empty cache. *)

val read :
  peer:string    ->
  service:string ->
  file:string    ->
  t              -> (Yojson.Basic.json * t) option
(** If [file] on [service] at [peer] is cached return [Some (content,cache)]
    where [content] is the JSON file contents and [cache] is the data cache with
    LRU statistics updated for this read. Otherwise return [None]. *)

val write :
  peer:string               ->
  service:string            ->
  file:string               ->
  content:Yojson.Basic.json ->
  t                         -> t
(** Caches JSON [content] to the data cache for [file] on [service] at [peer].
    Return value is the updated data cache with [content] cached and LRU
    statistics updated. *)

val invalidate :
  peer:string    ->
  service:string ->
  file:string    ->
  t              -> t
(** Invalidates [file] on [service] at [peer] in data cache if it exists,
    removing the data cache entry. If it doesn't exist no action is taken.
    Either case returns a data cache guaranteed to not contain anything under
    this key. *)
