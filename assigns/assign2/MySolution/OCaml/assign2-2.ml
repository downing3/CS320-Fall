#use "./../../../../classlib/OCaml/MyOCaml.ml";;

exception Mylist_subscript_exn;;

type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist
;;

let mylist_subscript_exn () = raise Mylist_subscript_exn;;

let rec mylist_length (xs: 'a mylist): int =
  match xs with
  | MyNil -> 0 
  | MyCons(_, xs) -> 1 + mylist_length xs  
  | MySnoc(xs, _) -> 1 + mylist_length xs  
  | MyReverse(xs) -> mylist_length xs
  | MyAppend2(xs1, xs2) -> mylist_length xs1 + mylist_length xs2
;;

let rec mylist_reverse (xs: 'a mylist): 'a mylist =
  match xs with
  | MyNil -> MyNil
  | MyCons (x, xs) -> MySnoc (mylist_reverse xs, x)
  | MySnoc (xs, x) -> MyCons (x, mylist_reverse xs)
  | MyReverse (xs) -> xs  
  | MyAppend2 (xs1, xs2) -> MyAppend2 (mylist_reverse xs2, mylist_reverse xs1)
;;

let rec mylist_get_at (xs: 'a mylist)(i0: int): 'a =
  match xs, i0 with
  | _, i0 when i0 < 0 -> mylist_subscript_exn ()
  | MyNil, _ -> mylist_subscript_exn ()
  | MyCons(x, _), 0 -> x
  | MyCons(_, xs), i0 -> mylist_get_at xs (i0 - 1)
  | MySnoc(xs, x), i0 -> 
      let len = mylist_length xs in
      if i0 = len then x else mylist_get_at xs i0
  | MyReverse(xs), i0 ->
      let rev_xs = mylist_reverse xs in
      mylist_get_at rev_xs i0
  | MyAppend2(xs1, xs2), i0 ->
      let len_xs1 = mylist_length xs1 in
      if i0 < len_xs1 then mylist_get_at xs1 i0
      else mylist_get_at xs2 (i0 - len_xs1)
;;
