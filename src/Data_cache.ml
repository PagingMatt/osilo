module V = struct
  type t = Yojson.Basic.json
  let weight j =
    Yojson.Basic.to_string j
    |> String.length
end

module C = Lru.F.Make(Base.String)(V)

type t = C.t

let create = C.empty 1000000 (* 1MB cache assuming 1 byte characters *)

let build_path ~peer ~service ~file =
  Printf.sprintf "%s/%s/%s" peer service file

let read ~peer ~service ~file =
  C.find (build_path ~peer ~service ~file)

let write ~peer ~service ~file ~content =
  C.add (build_path ~peer ~service ~file) content

let invalidate ~peer ~service ~file =
  C.remove (build_path ~peer ~service ~file)
