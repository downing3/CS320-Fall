(*
Assign3-3:
HX-2023-09-26: 10 points
//
The function [list_nchoose(xs)(n0)]
returns all the subsequences of xs that are
of length n0.
//
let rec
list_nchoose
(xs: 'a list)(n0: int): 'a list list =
//
Please give a NON-RECURSIVE implementation of
list_nchoose based on list-combinators. Note that
the order of the elements in a list representation
of a subsequenc is SIGNIFICANT. For instance, [1;2]
and [2;1] are DIFFERENT.
//
*)
let len l = List.fold_left (fun acc _ -> acc + 1) 0 l;;

let list_nchoose (xs: 'a list) (n0: int): 'a list list =
  if n0 = 0 then [[]]
  else if n0 > len xs then []
  else
      (* this generates all combinations of the list *)
      let all_combinations = 
        List.fold_left (fun acc x ->
          acc @ (List.map (fun subset -> x :: subset) acc)
        ) [[]] xs in
        (* this filters out only the ones of the correct length *)
      List.filter (fun subset -> List.length subset = n0) all_combinations
    



