exception MySubscript

let mylist_subscript_exn () : 'a = raise MySubscript;;

type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist
;;

let rec mylist_get_at (xs: 'a mylist) (i0: int): 'a =
  if i0 < 0 then mylist_subscript_exn ()
  else match xs with
    | MyNil -> mylist_subscript_exn ()
    | MyCons (hd, tl) ->
      if i0 = 0 then hd
      else mylist_get_at tl (i0 - 1)
    | MySnoc (tl, last) ->
      let len = mylist_length tl in
      if i0 = len then last
      else if i0 < len then mylist_get_at tl i0
      else mylist_subscript_exn ()
    | MyReverse tl -> 
      let len = mylist_length tl in
      mylist_get_at tl (len - i0 - 1)
    | MyAppend2 (l1, l2) ->
      let len1 = mylist_length l1 in
      if i0 < len1 then mylist_get_at l1 i0
      else mylist_get_at l2 (i0 - len1)

and mylist_length (xs: 'a mylist) : int = 
  match xs with
  | MyNil -> 0
  | MyCons (_, tl) -> 1 + (mylist_length tl)
  | MySnoc (tl, _) -> 1 + (mylist_length tl)
  | MyReverse tl -> mylist_length tl
  | MyAppend2 (l1, l2) -> (mylist_length l1) + (mylist_length l2)
;;
