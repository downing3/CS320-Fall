#use "../../../../classlib/OCaml/MyOCaml.ml";;

let replace_char_at_index (str: string) (index: int) (new_char: char): string =
  let len = String.length str in
  let result = String.init len (fun i ->
    if i = index then new_char else str.[i]
  ) in
  result

  let rec concatenate_sublists lst = 
    match lst with
    | [] -> []
    | head::tail -> List.append head (concatenate_sublists tail)
  ;;

let list_of_buddies(word: string): string list = 
  let alphabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'] in
  
  let buddies_of_char_index index =
    let orig_char = word.[index] in
    (* excludes the original character *)
    let replacement_chars = List.filter (fun c -> c <> orig_char) alphabet in
    List.map (fun replacement -> replace_char_at_index word index replacement) replacement_chars
  in
  
  (* Create a list of all possible character indices in the word *)
  let all_indices = List.init (String.length word) (fun i -> i) in
    concatenate_sublists (List.map buddies_of_char_index all_indices)
;;







