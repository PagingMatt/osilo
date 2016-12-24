module M : sig
    include Macaroons.S
end
(** Instantiates Macaroons functor with CRYPTO over Nocrypto. *)

module Token : sig
  type t = R | W 
  (** Tokens are either read [R] or write [W]. Unlike UNIX write implies read. *)

  exception Invalid_token of string
  (** Raised when [token_of_string] is called on an invalid input. *)

  val token_of_string : string -> t
  (** Used to deserialise a string representing a token. "R" -> [R] and "W" -> [W]. Anything else
  raises a [Invalid_token]. *)

  val string_of_token : t -> string
  (** Serialises [Token.t] to [string] [R] -> "R" and [W] -> "W". *)

  val (>>) : t -> t -> bool
  (** Expresses strictly greater than relation over two inputs, for [t1 >> t2], true if [t1] is 
  strictly more powerful then [t2]. This can only return true if called with [W >> R]. *)

  val (>=) : t -> t -> bool
  (** Expresses greater than or equal to relation over two inputs, for [t1 >= t2], true for 
  [R >= R], [W >= R] and [W >= W].*)
end
(** Permissions tokens. These are used to express the difference between being able to read and 
write on remote peers, although currently only remote reading is implemented. *)

module CS : sig 
  type t 

  val empty : t

  val record_if_most_general : 
    service:t          ->
    permission:Token.t -> 
    macaroon:M.t       -> t

  val find_most_general_capability :
    service:t          ->
    path:string        ->
    permission:Token.t -> (Token.t * M.t) option
end

val authorise : (string list) -> (M.t list) -> Token.t -> Cstruct.t -> Peer.t -> string -> (string list)
(** [authorise paths capabilities token key target service] returns the subset of [paths] which is
covered by [capabilities] for a request of level [token]. [key] is the key used to mint each 
element of capabilities, [target] is the server's data being read (always this server) and 
[service] is the service that the elements of [paths] are on. *)

val verify : Token.t -> string -> M.t -> bool
(** [verify token key capability] verifies that [capability] was minted with [key] and that it 
holds a permission token of at least [token] as a first party caveat. *)

val mint : Peer.t -> Cstruct.t -> string -> (string * string) list -> (string * M.t) list
(** [mint source key service permissions] takes each element of [permissions] and builds a list of
string tokens and Macaroons tuples. Each Macaroon hold a first party caveat of the token it is in 
the tuple with and had a location of [source]/[service]/[path] where [path] is from an element of
[permissions]. Each Macaroon is signed with [key], this servers secret key. *)

val find_permissions : CS.t -> (Token.t * string) list -> M.t list
(** [find_permissions capabilities_service targets] builds up a list of Macaroons which cover the 
path of each element of [targets] which are at least as powerful as the [Token.t] paired with the 
target path. This uses a greedy approach to build a minimal covering set. *)

val record_permissions : CS.t -> (Token.t * M.t) list -> CS.t
(** [record_permissions capabilities_service targets] takes each element in [targets] and inserts
it and the paired [Token.t] into [capabilities_service] if [capabilities_service] does not already
contain a more general element which is at least as powerful as this element. *)

val serialise_request_capabilities   : M.t list -> Yojson.Basic.json
(** Serialises a list of capabilities to accompany a get request. This is [Yojson.Basic.json] as it
will then be composed with other JSON. *)

val deserialise_request_capabilities : Yojson.Basic.json -> M.t list
(** Deserialises a JSON collection of capabilities accompanying a get request into a list of [M.t]. *)

val serialise_presented_capabilities : (string * M.t) list -> string
(** Serialises a list of permission, Macaroon pairs into a string to send to another peer. These
are capabilities for this peer that are being given to the target peer. *)

val deserialise_presented_capabilities : string -> (Token.t * M.t) list
(** Deserialises a string of permission, Macaroon pairs that have been send to give capabilities on
the source peer. *)
