#use "../../../../classlib/OCaml/MyOCaml.ml";;

let len l = List.fold_left (fun acc _ -> acc + 1) 0 l;;

let list_subsets (xs: 'a list): 'a list list =
  let list_length = len xs in
  if list_length = 0 then [[]]
  else if list_length = 1 then [xs]
  else if list_length = 2 then [xs]
  else
    let base = [[]] in

    List.fold_left (fun acc x ->
        acc @ (List.map (fun subset -> x :: subset) acc)
    ) base xs
;;





  
