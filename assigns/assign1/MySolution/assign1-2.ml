#use "./../../../classlib/OCaml/MyOCaml.ml";;

let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0


let string_merge (cs1: string) (cs2: string): string =
  let len1 = String.length cs1 in
  let len2 = String.length cs2 in

  let rec merge_fwork i j work =
    if i < len1 && j < len2 then
      if cs1.[i] <= cs2.[j] then
        (work cs1.[i]; merge_fwork (i+1) j work)
      else
        (work cs2.[j]; merge_fwork i (j+1) work)
    else if i < len1 then
      (work cs1.[i]; merge_fwork (i+1) j work)
    else if j < len2 then
      (work cs2.[j]; merge_fwork i (j+1) work)
  in
  
  if len1 = 0 then cs2  (* handles empty strings *)
  else if len2 = 0 then cs1

  else string_make_fwork (fun work -> merge_fwork 0 0 work)
;;




