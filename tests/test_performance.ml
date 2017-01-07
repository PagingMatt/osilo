open Core.Std
open Core_bench.Std

let peer = "localhost" |> Peer.create
let key = "fooBARfooBARfooBARfooBARfooBARfo" |> Coding.decode_cstruct
let service = "foo"

open Auth.Token

let paths = ["erwfg";"werg";"wrt";"yjtr";"asd";"irut";"oiyt";"f3"]
let s = "R"
let t =  R

let tokpaths =
  List.map paths ~f:(fun p -> (s,p))

let worst_case_args = 
  List.map paths ~f:(fun p -> (t,p))

let capabilities =
  Auth.mint peer key service tokpaths

let _,capabilities' = List.unzip capabilities

let tree = 
  List.fold ~init:Auth.CS.empty capabilities
    ~f:(fun s' -> fun (t',c') -> Auth.CS.record_if_most_general s' (t' |> token_of_string) c')

let () = Command.run (Bench.make_command [
  Bench.Test.create_indexed
    ~name:"Building CS tree"
    ~args:[1;2;3;4;5;6;7;8]
    (fun num -> Staged.stage 
      (fun () -> ignore (List.fold ~init:Auth.CS.empty (List.take capabilities num)
        ~f:(fun s' -> fun (t',c') -> Auth.CS.record_if_most_general s' (t' |> token_of_string) c'))));
  Bench.Test.create_indexed
    ~name:"Verifying capabilities"
    ~args:[1;2;3;4;5;6;7;8]
    (fun num -> Staged.stage 
      (fun () -> 
        ignore (List.map (List.take capabilities' num) ~f:(Auth.verify R (key |> Coding.encode_cstruct)))));
  Bench.Test.create_indexed
    ~name:"Worst case capability selection"
    ~args:[1;2;3;4;5;6;7;8]
    (fun num -> Staged.stage 
      (fun () -> ignore (Auth.find_permissions tree (List.take worst_case_args num))));
  Bench.Test.create_indexed
    ~name:"Worst case capability verification"
    ~args:[1;2;3;4;5;6;7;8]
    (fun num -> Staged.stage 
      (fun () -> ignore (Auth.authorise (List.take paths num) (List.take capabilities' num) R key peer service)))
])