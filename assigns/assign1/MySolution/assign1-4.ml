#use "./../../../classlib/OCaml/MyOCaml.ml";;

let intrep_add (ds1: string) (ds2: string): string =

    let add int1 int2 carry = 
        let sum = int1 + int2 + carry in
        (sum mod 10, sum / 10) 
    in

    let string_of_result fwork = 
        let len1 = String.length ds1 in
        let len2 = String.length ds2 in
        let max_len = max len1 len2 in

        let rec process_list chars =
            match chars with
            | [] -> ()
            | h :: t -> fwork h; process_list t
        in

        let rec iterate i carry acc = 
            if i < max_len || carry != 0 then
                let char1 = if (len1 - 1 - i) >= 0 then string_get_at ds1 (len1 - 1 - i) else '0' in
                let char2 = if (len2 - 1 - i) >= 0 then string_get_at ds2 (len2 - 1 - i) else '0' in
                let num1 = digit_of_char char1 in
                let num2 = digit_of_char char2 in
                let (current_sum, new_carry) = add num1 num2 carry in
                let new_acc = Char.chr (current_sum + 48) :: acc in 
                iterate (i + 1) new_carry new_acc
            else 
                process_list acc
        in

        iterate 0 0 []
    in

    string_make_fwork string_of_result

