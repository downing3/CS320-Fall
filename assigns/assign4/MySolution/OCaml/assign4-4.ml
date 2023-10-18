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

let list_map(input_list: 'a list)(func: 'a -> 'b): 'b list =
  list_foldright(input_list)([])(fun elem accum -> func(elem) :: accum)

let list_cons(element: 'a)(input_list: 'a list): 'a list =
  element :: input_list

let rec stream_map stream_elem transform_func fallback_func =
  match stream_elem with
  | StrNil -> fallback_func ()
  | StrCons (value, next_func) -> StrCons(transform_func value, fun () -> stream_map (next_func ()) transform_func fallback_func)

let rec perms (input_list: 'a list): 'a list stream = 
  match input_list with
  | [] -> fun () -> StrCons([], fun () -> StrNil)
  | _ -> let rec inner_perms(pre_list: 'a list) (post_list: 'a list) =
    match pre_list with
    | [] -> StrNil
    | head::tail -> stream_map (perms (list_reverse post_list @ tail) ()) (list_cons head) (fun () -> inner_perms tail (head::post_list))
  in
    fun() -> inner_perms input_list []

let list_permute(input_list: 'a list): 'a list stream =
  perms(input_list)
;;



















