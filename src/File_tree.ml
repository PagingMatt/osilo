type 'a t =
    | Node of string * ('a option) * ('a t) * ('a t) * ('a t)
    | Leaf

exception Path_empty

let empty = Leaf

let insert ~element ~tree ~location ~select =
  let location' = location element in
  let rec ins path tree' =
    match path with
    | []    -> raise Path_empty
    | x::[] -> 
        (match tree' with 
        | Leaf -> Node (x, Some element, Leaf, Leaf, Leaf)
        | Node (name, el, sub, l, r) -> 
            if name > x then Node (name, el, sub, (ins path l), r) else
            if name < x then Node (name, el, sub, l, (ins path r)) else
            (match el with
            | None     -> Node (name, Some element, sub, l, r)
            | Some el' -> Node (name, Some (select el' element), sub, l, r)))
    | y::ys -> 
        match tree' with
        | Leaf -> Node (y, None, (ins ys Leaf), Leaf, Leaf)
        | Node (name,el,sub,l,r) -> 
            if name > y then Node (name, el, sub, (ins path l), r) else
            if name < y then Node (name, el, sub, l, (ins path r)) else
            Node (name, el, (ins ys sub), l, r)
  in ins location' tree

let shortest_path_match ~tree ~location ~satisfies =
  let rec find path tree =
      match path with 
      | []    -> None
      | x::[] ->
          (match tree with
          | Leaf -> None
          | Node (name, el, sub, l, r) -> 
              if name > x then find path l else
              if name < x then find path r else
              match el with
              | None          -> None
              | Some el' as e -> if satisfies el' then e else None)
      | y::ys ->
          match tree with 
          | Leaf -> None
          | Node (name,el,sub,l,r) -> 
              if name > y then find path l else
              if name < y then find path r else
              (match el with
              | None         -> find ys sub
              | Some el' as e-> if satisfies el' then e else find ys sub)
  in find location tree

let flatten_under ~tree ~path = []