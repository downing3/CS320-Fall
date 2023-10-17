#use "./../../assign4.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a gtree =
| GTnil | GTcons of 'a * ('a gtree list)

(* ****** ****** *)

(*
//
Assign4-3:
//
HX-2023-10-05: 10 points
//
Please enumerate a gtree in the manner of
depth-first search:
//
let rec (* 5 points *)
gtree_streamize_dfs(xs: 'a gtree): 'a stream
//
Please enumerate a gtree in the manner of
breadth-first search:
//
let rec (* 5 points *)
gtree_streamize_bfs(xs: 'a gtree): 'a stream
//
*)

(* ****** ****** *)


let rec gtree_streamize_dfs (xs: 'a gtree): 'a stream =
  let rec dfs_stack stack () =
    match stack with
    | [] -> StrNil
    | GTnil :: rest -> dfs_stack rest ()
    | GTcons (x, children) :: rest ->
      let child_stream = gtree_streamize_dfs_list children in
      StrCons (x, fun () -> dfs_stack (child_stream @ rest) ())
  and gtree_streamize_dfs_list nodes =
    match nodes with
    | [] -> []
    | node :: rest -> node :: gtree_streamize_dfs_list rest
  in
  match xs with
  | GTnil -> fun () -> StrNil
  | GTcons (x, children) -> fun () -> StrCons (x, fun () -> dfs_stack children ())

let rec gtree_streamize_bfs (xs: 'a gtree): 'a stream =
  let rec bfs_queue queue () =
    match queue with
    | [] -> StrNil
    | GTnil :: rest -> bfs_queue rest ()
    | GTcons (x, children) :: rest ->
      let child_stream = gtree_streamize_bfs_list children in
      StrCons (x, fun () -> bfs_queue (rest @ child_stream) ())
  and gtree_streamize_bfs_list nodes =
    match nodes with
    | [] -> []
    | node :: rest -> node :: gtree_streamize_bfs_list rest
  in
  match xs with
  | GTnil -> fun () -> StrNil
  | GTcons (x, children) -> fun () -> StrCons (x, fun () -> bfs_queue children ())







