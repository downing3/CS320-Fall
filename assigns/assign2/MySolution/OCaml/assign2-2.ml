#use "./../../../../classlib/OCaml/MyOCaml.ml";;

exception Mylist_subscript_exn


type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist

let rec mylist_length (xs: 'a mylist): int =
  match xs with
  | MyNil -> 0 
  | MyCons(_, xs) -> 1 + mylist_length xs  
  | MySnoc(xs, _) -> 1 + mylist_length xs  
  | MyReverse(xs) -> mylist_length xs
  | MyAppend2(xs1, xs2) -> mylist_length xs1 + mylist_length xs2

let mylist_subscript_exn () = raise Mylist_subscript_exn

let rec mylist_reverse (xs: 'a mylist): 'a mylist =
  match xs with
  | MyNil -> MyNil
  | MyCons (x, xs) -> MySnoc (mylist_reverse xs, x)
  | MySnoc (xs, x) -> MyCons (x, mylist_reverse xs)
  | MyReverse (xs) -> xs  
  | MyAppend2 (xs1, xs2) -> MyAppend2 (mylist_reverse xs2, mylist_reverse xs1)  
  
let rec mylist_get_at (xs: 'a mylist)(i0: int): 'a =
  let listlen = mylist_length xs in
  if i0 >= listlen then mylist_subscript_exn () else
      match xs with
      | MyNil -> mylist_subscript_exn ()  
      | MyCons (x, xs) ->
          if i0 = 0 then x else mylist_get_at xs (i0 - 1)
      | MySnoc (xs, x) ->
          let taillen = mylist_length xs in
          if i0 = taillen then x else mylist_get_at xs i0
      | MyReverse (xs) ->
          mylist_get_at (mylist_reverse xs) (listlen - i0 - 1)  
      | MyAppend2 (xs1, xs2) ->
          let len_xs1 = mylist_length xs1 in
              if i0 < len_xs1 then mylist_get_at xs1 i0 else mylist_get_at xs2 (i0 - len_xs1)







  

      


