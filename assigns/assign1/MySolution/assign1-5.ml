#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_longest_ascend(xs: string): string =

  let substring s start length = 
    string_init length (fun i -> string_get_at s (start + i))
  in

  let len = string_length xs in

  if len = 0 then ""
  else 
    let rec helper i currStart longestStart longestLen =
      if i = len then
        if i - currStart > longestLen then
          substring xs currStart (i - currStart)
        else 
          substring xs longestStart longestLen
      else
        if string_get_at xs i >= string_get_at xs (i - 1) then
          helper (i + 1) currStart longestStart longestLen
        else 
          if i - currStart > longestLen then
            helper (i + 1) i currStart (i - currStart)
          else 
            helper (i + 1) i longestStart longestLen
    in
    helper 1 0 0 0
;;

