open Nocrypto

exception Key_exchange_failed

type message = 
	| P of Cstruct.t
	| C of Cstruct.t 

module HTTP : sig 
	val init_dh : peer:Peer.t -> public:Cstruct.t -> group:Dh.group -> Cstruct.t Lwt.t
end = struct 
	let init_dh ~peer ~public ~group =
		let uri = Uri.make ~scheme:"http" ~host:(Peer.host peer) ~port:(Peer.port peer) ~path:"/kx/init" in 
		let body = `Assoc [
			("public", `String (public |> Base64.encode    |> Cstruct.to_string)); 
			("group",  `String (group  |> Dh.sexp_of_group |> Sexp.to_string   ))
		] |> Yojson.Basic.to_string in 
		Client.post ~body uri >>= fun (r,b) -> 
			let code = r |> Response.status |> Code.code_of_status in 
				if code=200 
				then Cohttp_lwt_body.to_string body >>= fun body ->
					let json_body = Yojson.Basic.from_string body in 
					match Yojson.Basic.Util.member "public" json_body with
					| `String public -> 
						match public |> Cstruct.of_string |> Base64.decode with
						| Some public' -> public'
						| None         -> raise Key_exchange_failed
					| _  -> raise Key_exchange_failed
				else raise Key_exchange_failed
end

module KC : sig
	type t

	val empty : size:int -> t

	val remove : t -> Peer.t -> t

	val lookup : t -> Peer.t -> Cstruct.t option

	val add : t -> Peer.t -> Cstruct.t -> t
end = struct 
	module C = Lru_map.F.Make(Peer)

	type t = {
		cache : Cstruct.t C.t ;
		size  : int           ;
	}

	let empty ~capacity = 
		{ cache = C.empty ; size = capacity ;}

	let remove c p =
		C.remove p c.cache 

	let lookup c p =
		match C.find p c.cache with
		| Some (v,_) -> Some v
		| None       -> None

	let add c p k =
		c.cache
		|> C.add p k  
		|> C.trim c.size
end

module KS : sig
	type t

	val invalidate : cache:KC.t -> peer:Peer.t -> KC.t 

	val mediate : cache:KC.t -> peer:Peer.t -> public:Cstruct.t -> KC.t * Cstruct.t

	val lookup : cache:KC.t -> peer:Peer.t -> Cstruct.t option

	val lookup' : cache:KC.t -> peer:Peer.t -> (KC.t * Cstruct.t) Lwt.t
end = struct
	type t = {
		cache  : KC.t ;
	}

	let invalidate ~ks ~peer = KC.remove ks.cache peer

	let mediate ~ks ~peer ~group ~public' = 
		let secret, _ = Dh.gen_key group in 
		let shared    = Dh.shared group secret public' in
		let removed   = invalidate ks.cache peer in
		let added     = KC.add ks.cache peer secret in 
		added, shared

	let lookup ~ks ~peer = KC.lookup ks.cache peer

	let lookup' ~ks ~peer =
		match lookup ~ks ~peer with
		| Some key -> return (ks.cache, key)
		| None     -> 
			let group = Dh.gen_group 256 in
			let secret, public = Dh.gen_key group in 
			HTTP.init_dh public group >>= fun public' -> 
				match Dh.shared group secret public' with 
				| Some shared -> (KC.add ks.cache peer shared), shared
				| None        -> raise Key_exchange_failed 
end

module CS : sig
	val encrypt : cache:KC.t -> peer:Peer.t -> plaintext:Cstruct.t -> (KC.t * Cstruct.t * Cstruct.t) Lwt.t

	exception Decryption_failed

	val decrypt : cache:KC.t -> peer:Peer.t -> ciphertext:Cstruct.t -> iv:Cstruct.t -> Cstruct.t
end = struct
	let encrypt ~cache ~peer ~plaintext =
		KS.lookup' cache peer >>= fun cache', secret -> 
			let key             = Cipher_block.GCM.of_secret secret in 
			let iv              = Rng.generate
			 256 in 
			let ciphertext,tag  = Cipher_block.GCM.encrypt ~key ~iv plaintext in
			return (cache', ciphertext, iv)

	exception Decryption_failed

	let decrypt ~cache ~peer ~ciphertext ~iv =
		match KS.lookup cache peer with
		| Some secret ->
			let key           = Cipher_text.GCM.of_secret secret in 
			let plaintext,tag = Cipher_block.GCM.decrypt ~key ~iv ciphertext in
			plaintext
		| None        -> raise Decryption_failed
end

let () = Nocrypto_entropy_unix.initialize ()