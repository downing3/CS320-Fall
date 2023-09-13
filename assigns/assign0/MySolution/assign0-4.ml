let chr = Char.chr 
let str c0 = String.make 1 c0 
let string_length = String.length
let string_get cs i0 = String.get cs i0
let ord = Char.code

let str2int(cs: string): int = 
    let str_len = string_length cs in 

    let rec loop currposition result = 
        if currposition = str_len then result
        else
            let digit = ord (string_get cs currposition) - ord '0' in
            loop (currposition + 1) (result * 10 + digit)
    in
    loop 0 0
;;

(* ****** ****** *)
(*
My code takes a string as an input and returns an integer as an output. It does this recursively, but
iterating through the string, determining the ascii representation of the character, and then converting
that to an integer that is then added to the final output value. The first line of my recursive function
first checks the base case, if the current position is equal to the length of the string. If it is, then we 
know that we are at the end, and can simply return our accumulator variable. If not, we get to the iterative portion
of the function. This portion will extract the digit of the current character by using some of the functions we 
were given, string_get and ord. Because we are moving from place value to place value, we must take this into account
when calculating our final result. (result * 10) is what takes care of this. 
*)
(* ****** ****** *)