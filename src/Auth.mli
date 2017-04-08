(** Provides all capability operations, including managing capabilities other
peers have delegated out. *)

module Token : sig
  type t = R | W | D
  (** Tokens are either read [R], write [W] or delete [D]. Unlike UNIX write
    implies read etc. *)

  exception Invalid_token of string
  (** Raised when [token_of_string] is called on an invalid input. *)

  val token_of_string : string -> t
  (** Used to deserialise a string representing a token. "R" -> [R], "W" -> [W]
    and "D" -> [D]. Anything else raises a [Invalid_token]. *)

  val string_of_token : t -> string
  (** Serialises [Token.t] to [string] [R] -> "R", [W] -> "W" and [D] -> "D".
  *)

  val (>>) : t -> t -> bool
  (** Expresses strictly greater than relation over two inputs, for [t1 >> t2].
  *)

  val (>=) : t -> t -> bool
  (** Expresses greater than or equal to relation over two inputs.*)
end
(** Permissions tokens. These are used to express the difference between being
    able to read and write on remote peers. *)

module M : sig
  type t
  (** Internal type of capabilities is an instantiation of Macaroons. *)

  val create :
    source:Peer.t   ->
    service:string  ->
    path:string     ->
    delegate:Peer.t ->
    token:Token.t   ->
    key:Cstruct.t   -> t
  (** Create a new capability for a given location, presented to [delegate] and
      signed with [key]. *)

  val delegate : t -> Peer.t
  (** Get the delegate peer that the capability is for. *)

  val source   : t -> Peer.t
  (** Get the source peer that the capability can be used at. *)

  val service  : t -> string
  (** Get the service that the capability can be used on. *)

  val path     : t -> string
  (** Get the path down the file tree that the capability is recursively valid
      under. *)

  val location : t -> string
  (** Gets the [string] location the capability can be used at. This is of the
      form [source]/[service]/[path], taking [source] to be the [string]
      representation of the source peer. *)

  val token : t -> Token.t
  (** Gets the [Token.t] level of permission this capability gives. *)

  val verify :
    t                       ->
    required_service:string ->
    key:Cstruct.t           ->
    required:Token.t        ->
    this_peer:Peer.t        ->
    requester:Peer.t        -> bool
  (** Verifies that the given capability was signed by [key], and is for use at
      [this_peer], by [requester] on the service [required_service]. Finally it
      must be carrying a token of at least [required]. If and only if all this
      is satisfied, [true] is returned. *)

  exception Deserialisation_failed of string
  (** Raised in the case that a [string] cannot be deserialised into a
    capability. *)

  val string_of_t : t -> string
  (** Serialises a capability. *)

  val t_of_string : string -> t
  (** Deserialises a [string] to a capability or raises
      [Deserialisation_failed]. *)
end
(** Instantiates Macaroons functor with [CRYPTO] over [Nocrypto]. This also
    provides an abstraction over the values in the Macaroons to explicitly
    reference [delegate], [source], [service], [path] and [token]. *)

module CS : sig
  type t
  (** [File_tree.t] used to store permissions and Macaroons for nodes in a file
    tree. *)

  val empty : t
  (** Gives an empty capability service. *)

  val record_if_most_general :
    service:t          ->
    macaroon:M.t       -> t
  (** [record_if_most_general ~service ~macaroon] walks down [service] and if
      [macaroon] is more general and powerful than any other Macaroon along the
      path it gives the capability service with [macaroon] inserted, otherwise
      it just gives [service]. *)

  val find_most_general_capability :
    service:t          ->
    path:string        ->
    permission:Token.t -> M.t option
  (** [find_most_general_capability ~service ~path ~permission] finds the option
      of the most general capability along [path] in [service] which satisfies
      [permission], otherwise, [None]. *)

  val all_capabilities : t -> M.t list
  (** [all_capabilities service] gives every capability stored in [service]. *)
end
(** CS is the capability service, used to store capabilities given to this peer
    from other peers. *)

val authorise :
  string list ->
  M.t list    ->
  Token.t     ->
  Cstruct.t   ->
  Peer.t      ->
  string      ->
  Peer.t      -> string list
(** [authorise paths capabilities token key target service requester] returns
    the subset of [paths] which is covered by [capabilities] for a request of
    level [token]. [key] is the key used to mint each element of capabilities,
    [target] is the server's data being read (always this server) and [service]
    is the service that the elements of [paths] are on. *)

val covered : CS.t -> Token.t * string -> bool
(** [covered service (token,path)] gives [true] if some capability in [service]
    covers [path] with at least [token]. *)

val mint :
  minter:Peer.t ->
  key:Cstruct.t ->
  service:string ->
  permissions:(Token.t * string) list ->
  delegate:Peer.t -> M.t list
(** [mint ~minter ~key ~service ~permissions ~delegate] takes each element of
    [permissions] and builds a list of string tokens and capability tuples. Each
    Capability carries the token it is in the tuple with as well as [minter] as
    the source. It also has [service], the given path and the [delegate]. Each
    Capability is signed with [key]; this servers secret key. *)

val find_permissions :
  CS.t                    ->
  (Token.t * string) list ->
  Peer.t                  ->
  string                  -> M.t list * (Token.t * string) list
(** [find_permissions capabilities_service targets target_peer target_service]
    builds up a list of capabilities which cover the path of each element of
    [targets] which are at least as powerful as the [Token.t] paired with the
    [string] target path. This uses a greedy, optimal approach to build a
    minimal covering set. It returns this in a pair with a [list] of [Token.t]
    permission, [string] path pairs that couldn't be covered. *)

val record_permissions : CS.t -> M.t list -> CS.t
(** [record_permissions capabilities_service capabilities] takes each element in
    [targets] and inserts it paired with its [Token.t] permission level into
    [capabilities_service], if and only if [capabilities_service] does not
    already contain a more general element which is at least as powerful as this
    element in [Token.t]. *)

val vpath_subsumes_request : string -> string -> bool
(** [vpath_subsumes_request verified testing] returns [true] if [testing] is at
    or in a subdirectory of [verified]. Otherwise it is [false].*)
