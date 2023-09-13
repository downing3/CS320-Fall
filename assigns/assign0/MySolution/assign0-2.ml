let isPrime (n0: int): bool =
  if n0 <= 1 then
    false
  else
    let rec divisible x =
      if x * x > n0 then
        true
      else if n0 mod x = 0 then
        false
      else
        divisible (x + 1)
    in
    (divisible 2)
;;

let myans = isPrime(0)
;;

(* ****** ****** *)
(*
My code initially checks if the integer binded to n0 is greater than 1 (inclusive). If this is true, 
it moves onto a more in depth check thorugh a recursive function. Starting at 2, and ending once the square
root of the integer surpases n0, it checks if n0 is divisible by the current integer x. It does this using
the modulus operator. If it is not, it recursively increments by 1 onto the next integer. 
*)
(* ****** ****** *)
