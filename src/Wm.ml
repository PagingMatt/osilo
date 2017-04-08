(** Encapsulates an instantiation of [Webmachine]. *)

module Rd = Webmachine.Rd
include Webmachine.Make(Cohttp_lwt_unix_io)
