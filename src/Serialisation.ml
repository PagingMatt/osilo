module Message : sig
	type t
	exception Deserialisation_failed
	val serialise   : t -> Cstruct.t
	val deserialise : Cstruct.t -> t
end = struct
	type t =
		| Data_rd
		| Data_wr
		| Capa_rq
		| Capa_as

	exception Deserialisation_failed

	let serialise m =
		(match m with
		| Data_rd -> `Assoc [("type",`String "rd")]
		| Data_wr -> `Assoc [("type",`String "wr")]
		| Capa_rq -> `Assoc [("type",`String "rq")]
		| Capa_as -> `Assoc [("type",`String "as")]) |> Yojson.Basic.to_string |> Cstruct.of_string

    let deserialise m = 
        let s = Cstruct.to_string m in
		let j = Yojson.Basic.from_string s in
		match Yojson.Basic.Util.member "type" j with
		| `String x -> 
			match x with
			| "rd" -> Data_rd
			| "wr" -> Data_wr
			| "rq" -> Capa_rq
			| "as" -> Capa_as
			| _    -> raise Deserialisation_failed
		| _ -> raise Deserialisation_failed
end

