let chr = Char.chr
let str c0 = String.make 1 c0
let string_init = String.init
let string_length = String.length
let string_get = String.get
let ord = Char.code

let str_concat (s1: string)(s2: string): string =
  let len_s1 = string_length s1 in
  let len_s2 = string_length s2 in
  
  string_init (len_s1 + len_s2) (fun i ->
    if i < len_s1 then
      string_get s1 i
    else
      string_get s2 (i - len_s1)
  )

let int2str (i0: int): string =
  let rec converter n =
    if n < 10 then
      str (chr (ord '0' + n))
    else
      let currdigit = str (chr (ord '0' + (n mod 10))) in  
      str_concat (converter (n / 10)) currdigit
  in
  converter i0

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
