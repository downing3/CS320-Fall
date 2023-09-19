let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0

let stringrev(cs: string): string = 
  let str_len = string_length cs in
  string_init str_len (fun i -> string_get (cs, (str_len - 1 - i)))

(* ****** ****** *)
(*
My code takes a string as an input, and returns the reverse of the string. It does this by initially
determining the length of the string using the function given to us in class. It then uses string_init
to create a new string of str_len, a value we just found previously. I then use a lambda function that takes
an argument i, the current position in the string, and uses the string_get function to get the characters
at a given position, and place it into its new position. (str_len-1-i) is the calculation done to determine
which position in the new reversed string. This is correct becuase str_len-1 gives us the final index in the string, 
and subtracting i will give us its new position in respect to its original, i. 
*)
(* ****** ****** *)