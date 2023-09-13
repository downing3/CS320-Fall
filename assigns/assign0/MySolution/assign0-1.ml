let rec fact(x: int): int =
  if x > 0 then fact(x-1) * x else 1
;;

let rec myloop(x: int): int =
  if fact(x) = 0 then x else myloop(x+1)

let myans = myloop(0)
;;

(* ****** ****** *)
(*
The first integer that causes a stack overflow is 64.
*)
(* ****** ****** *)