type 'a t

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
  path:       string list ->
  satisfies: ('a -> bool) -> 'a option
(** [shortest_path_match ~tree ~path ~satisfies] starts at the root of [tree] and walks down 
towards [path] until an element along [path] in [tree] satisfies the predicate [satisfies], it then
returns this element. If it reaches a leaf before finding a satisfying element [None] is returned. *)

val flatten_under :
  tree: 'a t        ->
  path: string list -> 'a list
(** [flatten_under ~tree ~path] walks down [tree] until it hits [path] and then returns an in order
list of all of the elements at and below [path] in the [tree]. *)