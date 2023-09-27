#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let str_concat s1 s2 =
  let len_s1 = string_length s1 in
  let len_s2 = string_length s2 in
  string_init (len_s1 + len_s2) (fun i ->
    if i < len_s1 then
      string_get_at s1 i
    else
      string_get_at s2 (i - len_s1)
  )
;;

let rec list_fold_left f acc lst =
  match lst with
  | [] -> acc
  | x::xs -> list_fold_left f (f acc x) xs
;;

let string_sepjoin_list (sep: string) (xs: string list) : string =
  let helper acc elem =
    if acc = "" then elem
    else str_concat (str_concat acc sep) elem
  in
  match xs with
  | [] -> sep
  | [x] -> x
  | _ -> list_fold_left helper "" xs
;;




