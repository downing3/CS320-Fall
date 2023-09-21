#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_avoid_132(cs: string): bool =
  let len = string_length cs in
  
  let check i j k =
    cs.[i] < cs.[k] && cs.[k] < cs.[j]
  in

  let rec outer i =
    if i >= len then true
    else
      let rec middle j =
        if j >= len then true
        else
          let rec inner k =
            if k >= len then true
            else
              if check i j k then false
              else inner (k + 1)
          in
          if not (inner (j + 1)) then false
          else middle (j + 1)
      in
      if not (middle (i + 1)) then false
      else outer (i + 1)
  in

  outer 0
;;





