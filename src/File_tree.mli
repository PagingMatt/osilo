type 'a t

exception Path_empty
(** Raised when inserting and hit an empty path, this should never happen as the terminating
condition is on a singleton list, but if fed an empty path need to catch this. *)

val empty : 'a t 

val insert : 
  element:   'a                 -> 
  tree:      'a t               -> 
  location: ('a -> string list) -> 
  select:   ('a -> 'a -> 'a)    -> 'a t
(** [insert ~element ~tree ~location ~select] give the tree with [element] inserted into [tree], 
the place in the file tree that [element] is inserted into is determined by [location] which maps
the element to a list of directories which nest down to the target location. When at the target 
location, if this already exists, [select] compares [element] and the element currently held there
it then gives back the element which should be at the position. *)

val shortest_path_match :
  tree:       'a t        ->
  location:       string list ->
  satisfies: ('a -> bool) -> 'a option
(** [shortest_path_match ~tree ~location ~satisfies] starts at the root of [tree] and walks down 
towards [location] until an element along [location] in [tree] satisfies the predicate [satisfies], it then
returns this element. If it reaches a leaf before finding a satisfying element [None] is returned. *)

val flatten_under :
  tree: 'a t        ->
  location: string list -> 'a list
(** [flatten_under ~tree ~location] walks down [tree] until it hits [location] and then returns an in order
list of all of the elements at and below [location] in the [tree]. *)