#use "../../../../classlib/OCaml/MyOCaml.ml";;

(*
//
Assign4-4:
//
HX-2023-10-05: 20 points
//
Please enumerate all the permuations of a
given list. The enumeration is required to be
in order. For instance, say xs = [1;2;3], then
the enumeration should be of the following order
[1;2;3], [1;3;2], [2;1;3], [2;3;1], [3;1;2], [3;2;1].
//
let list_permute(xs: 'a list): 'a list stream
*)

(* ****** ****** *)

let rec permutations (xs: 'a list): 'a list list =
  match xs with
  | [] -> [[]]
  | x :: xs' ->
    let perms = permutations xs' in
    permute_insert_everywhere x perms []

and permute_insert_everywhere (x: 'a) (perms: 'a list list) (acc: 'a list list): 'a list list =
  match perms with
  | [] -> acc
  | perm :: perms' ->
    let new_perms = insert_everywhere x perm [] in
    permute_insert_everywhere x perms' (new_perms @ acc)

and insert_everywhere (x: 'a) (ys: 'a list) (acc: 'a list list): 'a list list =
  match ys with
  | [] -> (x :: []) :: acc
  | y :: ys' ->
    let new_perm = (x :: y :: ys') in
    insert_everywhere x ys' (new_perm :: acc)







