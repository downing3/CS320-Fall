(* ************************************************ *)

(*

 Question 8: 20 points
 Please give a NON-RECURSIVE implementation of sort5
 that takes 5 integers and returns a tuple that consists
 exactly of the 5 given integers ordered increasingly

 For instance, sort5(1, 2, 1, 2, 1) = (1, 1, 1, 2, 2)
 For instance, sort5(1, 3, 4, 5, 2) = (1, 2, 3, 4, 5)
 For instance, sort5(1, 3, 5, 4, 2) = (1, 2, 3, 4, 5)

 You can implement your own helper functions as long as
 you do not make use of recursion.

*)

let sort5 (a, b, c, d, e) =
  let swap a b = if a > b then (b, a) else (a, b) in
  
  let (a, b) = swap a b in
  let (b, c) = swap b c in
  let (c, d) = swap c d in
  let (d, e) = swap d e in

  let (a, b) = swap a b in
  let (b, c) = swap b c in
  let (c, d) = swap c d in

  let (a, b) = swap a b in
  let (b, c) = swap b c in

  let (a, b) = swap a b in

  (a, b, c, d, e)
  


(* ************************************************ *)
