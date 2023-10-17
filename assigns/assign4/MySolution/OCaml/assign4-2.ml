#use "../../../../classlib/OCaml/MyOCaml.ml";;

(*
//
Assign4-2:
//
HX-2023-10-05: 10 points
//
Please enumerate all the pairs of natural
numbers. Given pairs (i1, j1) and (i2, j2),
(i1, j1) should be enumerated ahead of (i2, j2)
if i1+j1 < i2+j2.
//
let theNatPairs: (int*int) stream = fun () -> ...
//
*)

(* ****** ****** *)

let theNatPairs: (int*int) stream = fun () -> 
    let rec generate_pairs s n =
      if n > s then 
        next_sum (s + 1) 0
      else
        StrCons((n, s - n), fun () -> generate_pairs s (n + 1))
  
    and next_sum s n = generate_pairs s n
  
    in
    next_sum 0 0

    


