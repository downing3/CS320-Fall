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

let rec insert_all_positions x ys =
  match ys with
  | [] -> StrCons([x], (fun () -> StrNil))
  | hd::tl -> 
      let next = fun () -> 
        let mapped = insert_all_positions x tl in
        match mapped with
        | StrNil -> StrNil
        | StrCons(lst, fn) -> StrCons(hd::lst, fn)
      in
      StrCons(x::hd::tl, next)

let rec list_permute xs =
  match xs with
  | [] -> (fun () -> StrCons([], fun () -> StrNil))
  | hd::tl ->
      let perms = list_permute tl in
      fun () -> 
        let sub_perms = perms () in
        match sub_perms with
        | StrNil -> StrNil
        | StrCons(perm, fn) -> insert_all_positions hd perm


















