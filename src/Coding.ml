let encode m = 
	m
	|> Nocrypto.Base64.encode
	|> Cstruct.to_string

exception Decoding_failed

let decode m =
	match m |> Cstruct.of_string |> Nocrypto.Base64.decode with
	| Some m' -> m'
	| None    -> raise Decoding_failed