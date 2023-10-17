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

let rec append_list xs ys =
    match xs with
    | [] -> ys
    | x::xs' -> x :: (append_list xs' ys)

let rec insert_all_positions x xs =s
    match xs with
    | [] -> [[x]]
    | y::ys -> 
        let rest = insert_all_positions x ys in
        (x::y::ys) :: (prepend y rest)
and prepend y lists = 
    match lists with
    | [] -> []
    | z::zs -> (y::z) :: (prepend y zs)

let rec list_permute (input_list: 'a list): 'a list stream =
    fun () -> 
        match input_list with
        | [] -> StrNil
        | x::xs ->
            let perms = list_permute xs in
            let rec interleave perms =
                match perms () with
                | StrNil -> StrNil
                | StrCons(lst, next) -> 
                    let all_inserted = insert_all_positions x lst in
                    let rec to_stream l = 
                        match l with
                        | [] -> interleave next
                        | h::t -> StrCons(h, fun () -> to_stream t)
                    in to_stream all_inserted
            in interleave perms









