#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

(* check if char is a digit *)
let is_digit c = c >= '0' && c <= '9'

(* convert list of digit chars into an integer *)
let rec digits_to_int acc = function
  | [] -> acc
  | h :: t -> digits_to_int (acc * 10 + (int_of_char h - int_of_char '0')) t

(* parse a number from a list of chars *)
let rec parse_number cs =
  match cs with
  | [] -> None
  | h :: t when is_digit h ->
    (* recursive helper function to parse digits *)
    let rec parse_digits ds cs =
      match cs with
      | h :: t when is_digit h -> parse_digits (ds @ [h]) t
      | _ -> (ds, cs) (* return list of digits and the rest of chars *)
    in
    let (digits, rest) = parse_digits [h] t in
    Some (Int (digits_to_int 0 digits), rest) (* convert digits to Int expression *)
  | _ -> None

let rec parse_expr cs =
  match trim cs with
  | '(' :: 'a' :: 'd' :: 'd' :: cs -> parse_add cs
  | '(' :: 'm' :: 'u' :: 'l' :: cs -> parse_mul cs
  | cs -> parse_number cs

(* parse ADD expression *)
and parse_add cs =
  match parse_exprs (trim cs) with
  | Some (exprs, ')' :: rest) -> Some (Add exprs, rest)
  | _ -> None

(* parse MUL expression *)
and parse_mul cs =
  match parse_exprs (trim cs) with
  | Some (exprs, ')' :: rest) -> Some (Mul exprs, rest)
  | _ -> None

(* parse list of expressions *)
and parse_exprs cs =
  let rec helper acc cs =
    (* use "parse_expr" to parse a single expression *)
    match parse_expr cs with
    | Some (expr, rest) -> helper (acc @ [expr]) (trim rest) (* append parsed expression *)
    | None when acc <> [] -> Some (acc, cs) (* return list of parsed expressions *)
    | None -> None
  in
  helper [] cs

let parse (s : string) : expr option =
  let chars = string_listize s in
  match parse_expr (trim chars) with
  | Some (expr, []) -> Some expr
  | _ -> None
