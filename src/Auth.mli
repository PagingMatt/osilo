module M : sig
    include Macaroons.S
end

module Token : sig
  type t = R | W 
  exception Invalid_token of string
  val token_of_string : string -> t
  val string_of_token : t -> string
  val (>>) : t -> t -> bool
  val (>=) : t -> t -> bool
end

val authorise : (string list) -> (M.t list) -> Token.t -> Cstruct.t -> Peer.t -> string -> (string list)

val verify : Token.t -> string -> M.t -> bool

val mint : Peer.t -> Cstruct.t -> string -> (string * string) list -> (string * M.t) list

val find_permissions : (Token.t * M.t) File_tree.t -> (Token.t * string) list -> M.t list

val record_permissions : (Token.t * M.t) File_tree.t -> (Token.t * M.t) list -> (Token.t * M.t) File_tree.t

val serialise_request_capabilities   : M.t list -> Yojson.Basic.json

val deserialise_request_capabilities : Yojson.Basic.json -> M.t list

val serialise_presented_capabilities : (string * M.t) list -> string

val deserialise_presented_capabilities : string -> (Token.t * M.t) list

