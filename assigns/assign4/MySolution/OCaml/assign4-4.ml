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

let list_permute(xs: 'a list): 'a list stream = 
  let rec helper lst =
    match lst with
    | [] -> StrNil
    | x :: rest ->
      let sub_perms = list_permute (rest @ [x]) in
      let prepend_to_each_list lst =
        List.map (fun perm -> x :: perm) lst
      in
      StrCons (x :: rest, fun () -> stream_map sub_perms prepend_to_each_list)
  in
  helper xs
  ;;




