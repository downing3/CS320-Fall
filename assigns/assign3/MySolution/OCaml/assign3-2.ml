#use "../../../../classlib/OCaml/MyOCaml.ml";;

let list_map = fun xs -> foreach_to_map_list(list_foreach)(xs)


let len l = List.fold_left (fun acc _ -> acc + 1) 0 l;;

let list_subsets (xs: 'a list): 'a list list =
  let list_length = len xs in
  if list_length = 0 then [[]]
  else if list_length = 1 then 
    match xs with
    | [x] -> [[x]; []]
    | _ -> failwith "Unexpected list length" 
  else if list_length = 2 then 
    match xs with
    | [x; y] -> [[x;y]; [x]; [y]; []]
    | _ -> failwith "Unexpected list length" 
  else
    let base = [[]] in
    List.fold_left (fun acc x ->
        acc @ (List.map (fun subset -> x :: subset) acc)
    ) base xs
;;








  
