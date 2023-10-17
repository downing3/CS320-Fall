#use "../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign4.ml";;


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

let rec list_permute (input_list: 'a list): 'a list stream =
  let rec generate_permutations lst =
    match lst with
    | [] -> StrNil
    | current_element :: rest ->
      let sub_perms = list_permute (rest @ [current_element]) in
      let prepend_to_list lst =
        let rec prepend_to_each acc prefix = function
          | [] -> acc
          | head :: tail ->
            let perm = prefix @ [head] @ tail in
            prepend_to_each (perm :: acc) prefix tail
        in
        prepend_to_each [] (current_element :: rest) lst
      in
      StrCons (current_element :: rest, fun () -> stream_map sub_perms prepend_to_list)
  in
  generate_permutations input_list
  ;;






