#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_avoid_1324(cs: string): bool = 
  let len = String.length cs in
  
  let rec helper i j k =
    if i >= len || j >= len || k >= len then true
    else
      let first = cs.[i] in
      let second = cs.[j] in
      let third = cs.[k] in
      if first < third && third < second then false
      else 
        if k + 1 < len then helper i j (k + 1)
        else if j + 1 < len then helper i (j + 1) (j + 2)
        else helper (i + 1) (i + 2) (i + 3)
  in

  helper 0 1 2
