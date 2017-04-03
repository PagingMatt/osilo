open Bench
open Bench.Bootstrap
open Core.Std

let peer = "localhost" |> Peer.create
let key = "fooBARfooBARfooBARfooBARfooBARfo" |> Coding.decode_cstruct
let service = "foo"

open Auth.Token

let number_paths = 4000
let paths =
  Nocrypto_entropy_unix.initialize ();
  List.init number_paths
    ~f:(fun _ ->
        Printf.sprintf "a/%s" (Nocrypto.Rng.generate 32
        |> Coding.encode_cstruct |> String.filter ~f:(fun c -> not(c='/')))
    )
let s = "R"
let t =  R

let bc_capability = Auth.mint ~minter:peer ~key ~service ~permissions:[(R,"a")] ~delegate:peer
let cap =
  match bc_capability with
  | c::_ -> c
  | _    -> assert false

let tokpaths =
  List.map paths ~f:(fun p -> (t,p))

let selection_args =
  List.map paths ~f:(fun p -> (t,Printf.sprintf "%s/%s/%s" (Peer.host peer) service p))

let capabilities =
  Auth.mint ~minter:peer ~key ~service ~permissions:tokpaths ~delegate:peer

let tree =
  List.fold ~init:Auth.CS.empty capabilities
    ~f:(fun s' -> fun c' -> Auth.CS.record_if_most_general s' c')

let tree' = Auth.CS.record_if_most_general (Auth.CS.empty) cap

let print_result r =
  Printf.printf "%s,%.16f,%.16f\n" r.desc r.mean.point r.stdev.point

let print_results = List.iter ~f:print_result

let best_sel args = bench_args (* Best case capability selection *)
  (fun num -> Auth.find_permissions tree' (List.take selection_args num) peer service)
  (List.map args ~f:(fun a -> Printf.sprintf "%d" a, a))

let worst_sel args = bench_args (* Worst case capability selection *)
  (fun num -> Auth.find_permissions tree (List.take selection_args num) peer service)
  (List.map args ~f:(fun a -> Printf.sprintf "%d" a, a))

let ver args = bench_args (* Verifying capabilities *)
  (fun num  -> List.map (List.take capabilities num) ~f:(Auth.M.verify ~required_service:service ~required:R ~key ~this_peer:peer ~requester:peer))
  (List.map args ~f:(fun a -> Printf.sprintf "%d" a, a))

let best_auth args = bench_args (* Best case capability verification *)
  (fun num -> ignore (Auth.authorise (List.take paths num) bc_capability R key peer service peer))
  (List.map args ~f:(fun a -> Printf.sprintf "%d" a, a))

let worst_auth args = bench_args (* Worst case capability verification *)
  (fun num -> ignore (Auth.authorise (List.take paths num) (List.take capabilities num) R key peer service peer))
  (List.map args ~f:(fun a -> Printf.sprintf "%d" a, a))

let () =
  Bench.config.Bench.samples <- 5000;
  let args = (List.range ~stride:50 ~start:`inclusive ~stop:`inclusive 1 number_paths) in
  print_results (best_sel args)
