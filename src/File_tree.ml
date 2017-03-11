type 'a t =
    | Node of string * ('a option) * ('a t) * ('a t) * ('a t)
    | Leaf

exception Path_empty

let empty = Leaf

let insert ~element ~tree ~location ~select ~terminate =
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
        | Node (name,el,sub,l,r) as node ->
            if name > y then Node (name, el, sub, (ins path l), r) else
            if name < y then Node (name, el, sub, l, (ins path r)) else
            if terminate el element then node else
            Node (name, el, (ins ys sub), l, r)
  in ins location' tree

let shortest_path_match ~tree ~location ~satisfies =
  let rec find path tree =
      match path with
      | []    -> None
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

let rec get_min tree =
  match tree with
  | Leaf -> Leaf
  | Node (_, _, _, Leaf, _) as n -> n
  | Node (_, _, _, l   , _)      -> get_min l

exception Trim_failed

let rec flatten ~tree =
  match tree with
  | Leaf -> []
  | Node (name, el, sub, l, r) ->
    (match el with
     | None   -> (flatten sub) @ (flatten l) @ (flatten r)
     | Some x -> x :: ((flatten sub) @ (flatten l) @ (flatten r)))

let rec flatten_below ~tree ~location =
  match location with
  | []    -> []
  | x::[] ->
    (match tree with
     | Leaf -> []
     | Node (name, el, sub, l, r) as n ->
       if name > x then flatten_below l (x::[]) else
       if name < x then flatten_below r (x::[]) else
       flatten n)
  | x::xs ->
    (match tree with
    | Leaf -> []
    | Node (name, el, sub, l, r) ->
      if name > x then flatten_below l (x::xs) else
      if name < x then flatten_below r (x::xs) else
        flatten_below sub xs)

let trim ~tree ~location =
  let flat el tree =
    match el with
    | None   ->       flatten tree
    | Some x -> x :: (flatten tree) in
  let rec delete_no_flatten path tree =
    match tree with
    | Leaf -> Leaf
    | Node (name, el, sub, l, r) ->
        if name > path then delete_no_flatten path l else
        if name < path then delete_no_flatten path r else
        if l = Leaf then r else
        if r = Leaf then l else
        let m = get_min r in
        (match m with
         | Node (n', e', s', Leaf, _) -> Node (n', e', s', l, delete_no_flatten n' r)
         | _ -> raise Trim_failed) in
  let rec delete path tree' =
    match path with
    | []    -> [],tree'
    | x::[] ->
        (match tree' with
        | Leaf -> [],Leaf
        | Node (name, el, sub, l, r) ->
            if name > x then let f,l' = delete path l in f,Node (name, el, sub, l', r) else
            if name < x then let f,r' = delete path r in f,Node (name, el, sub, l, r') else
            if l = Leaf then flat el sub,r else
            if r = Leaf then flat el sub,l else
            let m = get_min r in
            (match m with
            | Node (n', e', s', Leaf, _) -> flat el sub,Node (n', e', s', l, delete_no_flatten n' r)
            | _ -> raise Trim_failed))
    | y::ys ->
      match tree' with
      | Leaf -> [],Leaf
      | Node (name,el,sub,l,r) ->
          if name > y then let f,l' = delete path l in f,Node (name, el, sub, l', r) else
          if name < y then let f,r' = delete path r in f,Node (name, el, sub, l, r') else
          let f,sub' = delete ys sub in
          f,Node (name, el, sub', l, r)
  in delete location tree
