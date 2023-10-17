#use "./../../assign4.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a stream =
  | SNil
  | SCons of 'a * (unit -> 'a stream)


(* DFS *)
let rec gtree_streamize_dfs(xs: 'a gtree): 'a stream =
  match xs with
  | GTnil -> SNil
  | GTcons(x, children) ->
    SCons(x, fun () -> gtree_list_streamize_dfs children)

and gtree_list_streamize_dfs(l: 'a gtree list): 'a stream =
  match l with
  | [] -> SNil
  | hd::tl -> merge_streams (gtree_streamize_dfs hd) (fun () -> gtree_list_streamize_dfs tl)

and merge_streams s1 s2_gen =
  match s1 with
  | SNil -> s2_gen ()
  | SCons(x, tail_gen) -> SCons(x, fun () -> merge_streams (tail_gen ()) s2_gen)

(* BFS *)
let gtree_streamize_bfs(xs: 'a gtree): 'a stream =
let rec bfs q =
  match q with
  | [] -> SNil
  | GTnil::tl -> bfs tl
  | GTcons(x, children)::tl -> SCons(x, fun () -> bfs (tl @ children))
in
bfs [xs]

