module V = struct
  type t = Yojson.Basic.json
  let weight _ = 1
end

module C = Lru.F.Make(Core.Std.String)(V)

type t = C.t

let create = C.empty 500

let read = C.find

let write = C.add

let invalidate = C.remove
