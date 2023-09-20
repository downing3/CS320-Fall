#use "./../../../classlib/OCaml/MyOCaml.ml";;

let intrep_add(ds1: string)(ds2: string): string =

    let add int1 int2 carry = 
        let sum = int1 + int2 + carry in
        (sum mod 10, sum / 10) 
    in

    let rec iterate s1 s2 i carry fwork =
        let len1 = String.length s1 in
        let len2 = String.length s2 in
        let max_len = max len1 len2 in

        if i >= max_len then
            if carry = 1 then fwork (fun append -> append '1')
            else ()
        else
            let char1 = if i < len1 then string_get_at s1 (len1 - 1 - i) else '0' in
            let char2 = if i < len2 then string_get_at s2 (len2 - 1 - i) else '0' in
            let num1 = digit_of_char char1 in
            let num2 = digit_of_char char2 in
            let (current_sum, new_carry) = add num1 num2 carry in

            fwork (fun append -> append (char_of_int (current_sum + int_of_char '0')));
            iterate s1 s2 (i + 1) new_carry fwork
    in

    string_make_fwork (fun fwork -> iterate ds1 ds2 0 0 fwork)
