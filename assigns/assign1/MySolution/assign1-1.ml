(* ****** ****** *)
(*
assign1-1: 10 points
Given a natural number n that is not a multiple
of 10, intrev10(n) is the natural number whose
representation in base 10 reverses that of the
number n.

fun intrev10(n: int): int

For instance, if n = 12345, then intrev10(n) is
54321; if n = 10203, then intrev10(n) is 30201.

Please give a TAIL-RECURSIVE implementation of
intrev10.
*)
(* ****** ****** *)

let intrev10(n: int): int =
  let rec reverse num result =
    if num = 0 then result
    else
      let remainder = num mod 10 in
      reverse (num / 10) (result * 10 + remainder)
  in
  reverse n 0
;;


