let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0

let int2str (i0: int): string =
  let str_concat s1 s2 =
    let len_s1 = string_length s1 in
    let len_s2 = string_length s2 in

    string_init (len_s1 + len_s2) (fun i ->
      if i < len_s1 then
        string_get (s1, i)
      else
        string_get (s2, (i - len_s1)))
  in

  let is_negative = i0 < 0 in
  
  let rec converter n =
    if n < 10 then
      str (chr (ord '0' + n))
    else
      let currdigit = str (chr (ord '0' + (n mod 10))) in  
      str_concat (converter (n / 10)) currdigit
  in

  let result = converter (abs i0) in
  if is_negative then
    string_init (1 + string_length result) (fun i ->
      if i = 0 then
        chr 45 
      else
        string_get (result, i-1))
  else
    result

;;


(* ****** ****** *)
(*
My code takes an integer as an input, and outputs its string representation. I created a recursive function called converter
that takes as an input an integer n, the current digit, and outputlist, the resulting list of characters which will eventually
be our answer. The first line of this recursive function checks if the current digit is less than 10. This is important to do because 
if it is, we know it is a single digit and we can directly convert it without having to perform any mod calculations. So if true,
it takes the digit and converts it using chr (ord '0' + n). These make use of the functions given to use in this class. If the digit
is creater than 9, meaning it is a two-digit number, the mod operation will isolate the last digit of the number and converts it. 
This entire process will recursively occur on all digits. 
*)
(* ****** ****** *)
