#use "../../../../classlib/OCaml/MyOCaml.ml";;

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

type 'a gtree =
  | GTnil
  | GTcons of 'a * ('a gtree list)

type 'a strcon =
  | StrNil
  | StrCons of 'a * (unit -> 'a strcon)

type 'a stream =
  unit -> 'a strcon

let rec gtree_streamize_dfs (tree: 'a gtree): 'a stream =
  let rec dfs_helper node =
    match node with
    | GTnil -> fun () -> StrNil
    | GTcons (value, children) ->
      let children_streams = List.map dfs_helper children in
      let children_stream () =
        let rec unfold_child_streams = function
          | [] -> StrNil
          | f :: rest ->
            match f () with
            | StrNil -> unfold_child_streams rest
            | StrCons (x, xs) -> StrCons (x, fun () -> unfold_child_streams (xs () :: rest))
        in
        unfold_child_streams children_streams
      in
      fun () -> StrCons (value, children_stream)
  in
  dfs_helper tree ()

let rec gtree_streamize_bfs (tree: 'a gtree): 'a stream =
  let rec bfs_helper queue =
    match queue with
    | [] -> fun () -> StrNil
    | GTnil :: rest -> bfs_helper rest ()
    | GTcons (value, children) :: rest ->
      let child_values = List.map (fun child -> match child with GTnil -> failwith "Unexpected GTnil in children" | GTcons (v, _) -> v) children in
      let new_queue = rest @ children in
      fun () -> StrCons (value, bfs_helper new_queue)
  in
  match tree with
  | GTnil -> fun () -> StrNil
  | GTcons (value, children) ->
    fun () -> StrCons (value, bfs_helper [GTcons (value, children)] ())
