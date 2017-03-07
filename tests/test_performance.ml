open Bench
open Core.Std

let peer = "localhost" |> Peer.create
let key = "fooBARfooBARfooBARfooBARfooBARfo" |> Coding.decode_cstruct
let service = "foo"

open Auth.Token

let number_paths = 20000
let paths =
  Nocrypto_entropy_unix.initialize ();
  List.init number_paths
    ~f:(fun _ ->
        Printf.sprintf "a/%s" (Nocrypto.Rng.generate 32
        |> Coding.encode_cstruct |> String.filter ~f:(fun c -> not(c='/')))
    )
let s = "R"
let t =  R

let bc_capability = Auth.mint peer key service [(R,"a")]
let cap =
  match bc_capability with
  | c::_ -> c
  | _    -> assert false

let tokpaths =
  List.map paths ~f:(fun p -> (t,p))

let selection_args =
  List.map paths ~f:(fun p -> (t,Printf.sprintf "%s/%s/%s" (Peer.host peer) service p))

let capabilities =
  Auth.mint peer key service tokpaths

let tree =
  List.fold ~init:Auth.CS.empty capabilities
    ~f:(fun s' -> fun c' -> Auth.CS.record_if_most_general s' c')

let tree' = Auth.CS.record_if_most_general (Auth.CS.empty) cap

let () =
  let args = (List.range ~stride:250 ~start:`inclusive ~stop:`inclusive 1 number_paths) in
  let best_sel = bench_throughput (* Best case capability selection *)
    (fun num -> Auth.find_permissions tree' (List.take selection_args num)) args in
  let worst_sel = bench_throughput (* Worst case capability selection *)
    (fun num -> Auth.find_permissions tree (List.take selection_args num)) args in
  let ver = bench_throughput (* Verifying capabilities *)
    (fun num  -> List.map (List.take capabilities num) ~f:(Auth.verify R (key |> Coding.encode_cstruct))) args in
  let best_auth = bench_throughput (* Best case capability verification *)
      (fun num -> ignore (Auth.authorise (List.take paths num) bc_capability R key peer service)) args in
  let worst_auth = bench_throughput (* Worst case capability verification *)
      (fun num -> ignore (Auth.authorise (List.take paths num) (List.take capabilities num) R key peer service)) args in
  ()
