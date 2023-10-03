#use "../../../../classlib/OCaml/MyOCaml.ml";;

let len l = List.fold_left (fun acc _ -> acc + 1) 0 l;;

let list_nchoose (xs: 'a list) (n0: int): 'a list list =
  if n0 = 0 then [[]]
  else if n0 > len xs then []
  else
      (* this generates all combinations of the list *)
      let all_combinations = 
        List.fold_left (fun acc x ->
          acc @ (List.map (fun subset -> x :: subset) acc)
        ) [[]] xs 
      in
        (* this filters out only the ones of the correct length *)
      List.filter (fun subset -> len subset = n0) all_combinations
;;

    



