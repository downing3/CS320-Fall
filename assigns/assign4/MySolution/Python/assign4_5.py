#
# HX-2023-10-06: 30 points
# Please translate the following code from OCaml
# into Python:
#
# let
# string_fset_at
# (cs: string)(i0: int)(c0: char) =
# string_tabulate
# (string_length(cs))
# (
# fun i ->
# if i <> i0 then string_get_at(cs)(i) else c0)
# ;;
# (* ****** ****** *)
#
# let
# alphabet =
# string_tabulate(26)(fun i -> chr(ord('a') + i));;
#
# (* ****** ****** *)

def string_fset_at(cs, i0, c0):
    result = ""
    for i in range(len(cs)):
        if i != i0:
            result += cs[i]
        else:
            result += c0
    return result

alphabet = ''.join([chr(ord('a') + i) for i in range(26)])

def list_of_buddies(word):
    n0 = len(word)
    buddies = []
    for i0 in range(n0):
        c0 = word[i0]
        for c1 in alphabet:
            if c1 != c0:
                buddies.append(string_fset_at(word, i0, c1))
    return buddies

