module Message : sig
    type t = DH_INIT | DH_REPLY
    
    exception Unknown_message_type of string
    
    val of_string : string -> t
    val to_string : t -> string
end

module Json : sig
    type t = Yojson.Basic.json
    
    exception Json_deserialised_to_null of string
    exception Expected_string_member of string * t
    exception Expected_association_list_member of string * t
    
    val of_string_not_null : string -> t
    val member_str : t -> string -> string
    val member_asc : t -> string -> t
    val t_of_assoc_list : (string * t) list -> t
    val t_of_string : string -> t
    val string_of_message_body : Message.t -> t -> string
end

module type MESSAGE = sig 
  type t 

  val serialise : t -> string 
  val deserialise : string -> t 
end

exception Not_correct_message_type of Message.t * string

module Dh : sig
  exception B64_not_decodeable of string
  
  val decode_base64 : string -> Cstruct.t
  
  module Init : sig
    type t
    
    val serialise : t -> string
    val deserialise : string -> t
  end
  
  module Reply : sig
    type t
    
    val serialise : t -> string
    val deserialise : string -> t
  end
end
