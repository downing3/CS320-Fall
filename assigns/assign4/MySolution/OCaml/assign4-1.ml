#use "../../../../classlib/OCaml/MyOCaml.ml";;
exception Empty
(*
//
Assign4-1:
//
HX-2023-10-05: 10 points
//
The following is a well-known series:
ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...
Please implement a stream consisting of all the
partial sums of this series.
The 1st item in the stream equals 1
The 2nd item in the stream equals 1 - 1/2
The 3rd item in the stream equals 1 - 1/2 + 1/3
The 4th item in the stream equals 1 - 1/2 + 1/3 - 1/4
And so on, and so forth
//
let the_ln2_stream: float stream = fun() -> ...
//
*)

(* ****** ****** *)


let the_ln2_stream: float stream = fun () ->
  let rec aux n sum sign () =
    let next_sum = sum +. (sign /. float_of_int n) in
    StrCons (next_sum, aux (n + 1) next_sum (-. sign))
  in
  aux 1 0. 1. ()

let uncons(fxs) =
  match fxs() with
  | StrNil -> raise Empty
  | StrCons(x1, fxs) -> (x1, fxs)