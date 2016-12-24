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

let flatten_under ~tree ~location =
  let rec flatten tree' =
    match tree' with
    | Leaf -> []
    | Node (name, el, sub, l, r) -> 
      (match el with 
      | None   -> (flatten sub) @ (flatten l) @ (flatten r)
      | Some x -> x :: ((flatten sub) @ (flatten l) @ (flatten r)))
  in let rec find path tree' = 
    match path with 
      | []    -> []
      | x::[] ->
          (match tree' with
          | Leaf -> []
          | Node (name, el, sub, l, r) -> 
              if name > x then find path l else
              if name < x then find path r else
              (match el with
              | None -> flatten sub
              | Some x -> x :: (flatten sub)))
      | y::ys ->
          match tree' with 
          | Leaf -> []
          | Node (name,el,sub,l,r) -> 
              if name > y then find path l else
              if name < y then find path r else
              find ys sub
  in find location tree

let rec get_min tree = 
  match tree with
  | Leaf -> Leaf
  | Node (_, _, _, Leaf, _) as n -> n
  | Node (_, _, _, l   , _)      -> get_min l

exception Trim_failed

let trim ~tree ~location =
  let rec delete path tree' =
    match path with 
    | []    -> tree'
    | x::[] ->
        (match tree' with
        | Leaf -> Leaf
        | Node (name, el, sub, l, r) as target -> 
            if name > x then Node (name, el, sub, delete path l, r) else
            if name < x then Node (name, el, sub, l, delete path r) else
            if l = Leaf then r else
            if r = Leaf then l else
            let m = get_min r in
            (match m with
            | Node (n', e', s', Leaf, _) -> Node (n', e', s', l, delete [n'] r)
            | _ -> raise Trim_failed))
    | y::ys -> 
      match tree' with 
      | Leaf -> Leaf
      | Node (name,el,sub,l,r) -> 
          if name > y then Node (name, el, sub, delete path l, r) else
          if name < y then Node (name, el, sub, l, delete path r) else
          Node (name, el, delete ys sub, l, r)
  in delete location tree
