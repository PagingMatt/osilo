module V = struct
  type t = Yojson.Basic.json
  let weight _ = 1
end

module C = Lru.F.Make(Core.Std.String)(V)

type t = C.t

let create = C.empty 500

let build_path ~peer ~service ~file =
  Printf.sprintf "%s/%s/%s" peer service file

let read ~peer ~service ~file =
  C.find (build_path ~peer ~service ~file)

let write ~peer ~service ~file ~content =
  C.add (build_path ~peer ~service ~file) content

let invalidate ~peer ~service ~file =
  C.remove (build_path ~peer ~service ~file)
