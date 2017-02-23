module Token : sig
  type t = R | W | D
  (** Tokens are either read [R], write [W] or delete [D]. Unlike UNIX write implies read etc. *)

  exception Invalid_token of string
  (** Raised when [token_of_string] is called on an invalid input. *)

  val token_of_string : string -> t
  (** Used to deserialise a string representing a token. "R" -> [R], "W" -> [W] and "D" -> [D]. Anything else
  raises a [Invalid_token]. *)

  val string_of_token : t -> string
  (** Serialises [Token.t] to [string] [R] -> "R", [W] -> "W" and [D] -> "D". *)

  val (>>) : t -> t -> bool
  (** Expresses strictly greater than relation over two inputs, for [t1 >> t2]. *)

  val (>=) : t -> t -> bool
  (** Expresses greater than or equal to relation over two inputs.*)
end
(** Permissions tokens. These are used to express the difference between being able to read and
    write on remote peers, although currently only remote reading is implemented. *)

module M : sig
  type t
  val create :
    source:Peer.t   ->
    service:string  ->
    path:string     ->
    delegate:Peer.t ->
    token:Token.t   ->
    key:Cstruct.t   -> t
  val delegate : t -> Peer.t
  val source   : t -> Peer.t
  val service  : t -> string
  val path     : t -> string
  val location : t -> string
  val token : t -> Token.t
  val verify : t ->
    key:Cstruct.t    ->
    required:Token.t -> bool
  exception Deserialisation_failed of string
  val string_of_t : t -> string
  val t_of_string : string -> t
end
(** Instantiates Macaroons functor with CRYPTO over Nocrypto. *)

module CS : sig
  type t
  (** [File_tree.t] used to store permissions and Macaroons for nodes in a file tree. *)

  val empty : t
  (** Gives an empty capability service. *)

  val record_if_most_general :
    service:t          ->
    macaroon:M.t       -> t
  (** [record_if_most_general ~service ~macaroon] walks down [service] and if
  [macaroon] is more general and powerful than any other Macaroon along the path it gives the
  capability service with [macaroon] inserted, otherwise it just gives [service]. *)

  val find_most_general_capability :
    service:t          ->
    path:string        ->
    permission:Token.t -> M.t option
  (** [find_most_general_capability ~service ~path ~permission] finds the option of the most
  general capability along [path] in [service] which satisfies [permission], otherwise, None. *)

  val all_capabilities : t -> M.t list
end
(** CS is the capability service, used to store capabilities given to this peer from other peers. *)

val authorise : (string list) -> (M.t list) -> Token.t -> Cstruct.t -> Peer.t -> string -> (string list)
(** [authorise paths capabilities token key target service] returns the subset of [paths] which is
covered by [capabilities] for a request of level [token]. [key] is the key used to mint each
element of capabilities, [target] is the server's data being read (always this server) and
[service] is the service that the elements of [paths] are on. *)

val covered : CS.t -> Token.t * string -> bool

val mint : Peer.t -> Cstruct.t -> string -> (Token.t * string) list -> Peer.t -> M.t list
(** [mint source key service permissions delegate] takes each element of [permissions] and builds a list of
string tokens and Macaroons tuples. Each Macaroon's identifier is of the token it is in
the tuple with and had a location of [source]/[service]/[path] where [path] is from an element of
[permissions]. Each Macaroon is signed with [key], this servers secret key. *)

val find_permissions : CS.t -> (Token.t * string) list -> Peer.t -> string -> M.t list * (Token.t * string) list
(** [find_permissions capabilities_service targets] builds up a list of Macaroons which cover the
path of each element of [targets] which are at least as powerful as the [Token.t] paired with the
target path. This uses a greedy approach to build a minimal covering set. It returns this in a pair
with the permission path pairs that couldn't be covered. *)

val record_permissions : CS.t -> M.t list -> CS.t
(** [record_permissions capabilities_service targets] takes each element in [targets] and inserts
it and the paired [Token.t] into [capabilities_service] if [capabilities_service] does not already
contain a more general element which is at least as powerful as this element. *)

val vpath_subsumes_request : string -> string -> bool
(** [vpath_subsumes_request verified testing] returns [true] if [testing] is at or in a subdirectory
of [verified]. Otherwise it is [false].*)
