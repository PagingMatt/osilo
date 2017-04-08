(** Encapsulates the type for, and operations on, annotated file trees. *)

type 'a t
(** Internal type for annotated file trees, parameterised by the type of
    annotation. *)

exception Path_empty
(** Raised when inserting and hit an empty path, this should never happen as the
    terminating condition is on a singleton list, but if fed an empty path need
    to catch this. *)

val empty : 'a t
(** Value to get an empty file tree. *)

val insert :
  element:   'a                       ->
  tree:      'a t                     ->
  location: ('a -> string list)       ->
  select:   ('a -> 'a -> 'a)          ->
  terminate:('a option -> 'a -> bool) -> 'a t
(** [insert ~element ~tree ~location ~select ~terminate] give the tree with
    [element] inserted into [tree], provided for each step down the path,
    [terminate elopt el] returns false. The place in the file tree that
    [element] is inserted into is determined by [location] which maps the
    element to a list of directories which nest down to the target location.
    When at the target location, if this already exists, [select] compares
    [element] and the element currently held there it then gives back the
    element which should be at the position. *)

val shortest_path_match :
  tree:       'a t            ->
  location:       string list ->
  satisfies: ('a -> bool)     -> 'a option
(** [shortest_path_match ~tree ~location ~satisfies] starts at the root of
    [tree] and walks down towards [location] until an element along [location]
    in [tree] satisfies the predicate [satisfies], it then returns this element.
    If it reaches a leaf before finding a satisfying element [None] is returned.
    *)

exception Trim_failed
(** Raised if deletion behaves unexpectedly. *)

val flatten : tree:'a t -> 'a list
(** Flattens the entire [tree] from the root into a [list]. *)

val flatten_below : tree:'a t -> location:string list -> 'a list
(** Flattens [tree] from [location] into a [list]. *)

val trim :
  tree:    'a t         ->
  location: string list -> 'a list * 'a t
(** Walks down to the node at [location] in [tree] and returns the new tree with
    this node and it's sub tree removed, but any left and right nodes still
    remaining. The contents of the subtree is flattened and passed back in a
    pair. *)
