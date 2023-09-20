#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_avoid_132(cs: string): bool = 
  let len = String.length cs in

  let rec helper index =
    if index + 2 >= len then true
    else
      let first = cs.[index] in
      let second = cs.[index + 1] in
      let third = cs.[index + 2] in
      if first < third && third < second then false
      else helper (index + 1)
  in

  helper 0;;




