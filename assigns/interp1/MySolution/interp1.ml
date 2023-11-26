#use "./../../../classlib/OCaml/MyOCaml.ml";;

(* Type Definitions *)
type const = Int of int | Bool of bool | Unit
type command = Push of const | Pop | Trace | Add | Sub | Mul | Div | And | Or | Not | Lt | Gt
type stack = const list
type trace = string list

(* Helper Functions *)
let is_digit_or_minus ch =
   (ch >= '0' && ch <= '9') || ch = '-'

(* converts a constant to its string representation *)
let toString c = match c with
    | Int i -> string_of_int i
    | Bool b -> if b then "True" else "False"
    | Unit -> "Unit"

(* evaluates a constant *)
let eval_const c = match c with
    | Int i -> Int i
    | Bool b -> Bool b
    | Unit -> Unit

(* extracts top two integers from the stack *)
let top_two_ints stack =
    match stack with
    | Int i :: Int j :: _ -> Some (i, j)
    | _ -> None

(* maps a function over a list *)
let rec list_map f lst = 
    match lst with
    | [] -> []
    | h::t -> (f h) :: (list_map f t)

(* extracts a substring from a string *)
let substring str start len =
  if start < 0 || len < 0 || start + len > string_length str then
    failwith "substring: invalid bounds"
  else
    let rec build_substring i acc =
      if i < start + len then
        build_substring (i + 1) (string_snoc acc (string_get_at str i))
      else
        acc
    in build_substring start ""

(* splits a string into a list of strings *)
let rec string_split str seperator =
    let len = string_length str in
    let rec split acc last_pos pos =
        if pos < len then
            if string_get_at str pos = seperator then
                let next = if last_pos = pos then acc
                           else substring str last_pos (pos - last_pos) :: acc in
                split next (pos + 1) (pos + 1)
            else
                split acc last_pos (pos + 1)
        else
            let final = if last_pos = len then acc
                        else substring str last_pos (len - last_pos) :: acc in
            list_reverse final
    in split [] 0 0

(* converts string to int, specificallyn to parse numerical values from the program's input *)
let string_to_int str =
   let rec aux acc i =
     if i < 0 then acc
     else let ch = string_get_at str i in
          if ch = '-' then acc
          else let digit = digit_of_char ch in
               aux (acc + digit * int_of_float (10.0 ** float_of_int (string_length str - i - 1))) (i - 1)
   in
   let str_len = string_length str in
   if string_get_at str 0 = '-' then
     -1 * (aux 0 (str_len - 1))
   else
     aux 0 (str_len - 1)

 

(* Returns the tail of a list *)
let list_tail lst = match lst with
  | [] -> failwith "Empty list has no tail"
  | _ :: t -> t

(* check if list contains specific element *)
let rec list_contains lst x =
  match lst with
  | [] -> false
  | h :: t -> if h = x then true else list_contains t x

(* trims whitespace from the start and end of a string *)
let trim str = 
    let rec trim_start str =
        if str = "" then ""
        else if char_iswhitespace (string_get_at str 0) then trim_start (substring str 1 (string_length str - 1))
        else str
    in
    let rec trim_end str =
        if str = "" then ""
        else let len = string_length str in
        if char_iswhitespace (string_get_at str (len - 1)) then trim_end (substring str 0 (len - 1))
        else str
    in
    trim_end (trim_start str)

(* Parsing Constants and Commands *)
let parse_const str = 
   if str = "True" then Bool true
   else if str = "False" then Bool false
   else if str = "Unit" then Unit
   else if string_forall str is_digit_or_minus then Int (string_to_int str)
   else failwith "Invalid constant"

let parse_command str = 
   let parts = string_split (trim str) ' ' in
   match parts with
   | ["Push"; const_str] -> Push (parse_const const_str)
   | ["Pop"] -> Pop
   | ["Trace"] -> Trace
   | ["Add"] -> Add
   | ["Sub"] -> Sub
   | ["Mul"] -> Mul
   | ["Div"] -> Div
   | ["And"] -> And
   | ["Or"] -> Or
   | ["Not"] -> Not
   | ["Lt"] -> Lt
   | ["Gt"] -> Gt
   | _ -> failwith "Invalid command"

(* Parsing Programs *)
let parse_program program = 
   list_map parse_command (string_split program ';')

let eval_command cmd (stack, trace) = match cmd with
    | Push c -> (c :: stack, trace)
    | Pop -> (match stack with
                | _ :: s -> (s, trace)
                | [] -> ([], "Panic" :: trace))
    | Trace -> (match stack with
                | c :: s -> (Unit :: s, toString c :: trace)
                | [] -> ([], "Panic" :: trace))
    | Add -> (match top_two_ints stack with
                | Some (i, j) -> (Int (i + j) :: list_tail (list_tail stack), trace)
                | None -> ([], "Panic" :: trace))
    | Sub -> (match top_two_ints stack with  
                | Some (i, j) -> (Int (i - j) :: list_tail (list_tail stack), trace)
                | None -> ([], "Panic" :: trace))
    | Mul -> (match top_two_ints stack with
                | Some (i, j) -> (Int (i * j) :: list_tail (list_tail stack), trace)
                | None -> ([], "Panic" :: trace))
    | Div -> (match top_two_ints stack with  
                | Some (i, 0) -> ([], "Panic" :: trace) 
                | Some (i, j) -> (Int (i / j) :: list_tail (list_tail stack), trace)
                | None -> ([], "Panic" :: trace))
    | And -> (match stack with
                | Bool a :: Bool b :: s -> (Bool (a && b) :: s, trace)
                | _ -> ([], "Panic" :: trace))
    | Or -> (match stack with
                | Bool a :: Bool b :: s -> (Bool (a || b) :: s, trace)
                | _ -> ([], "Panic" :: trace))
    | Not -> (match stack with
                | Bool a :: s -> (Bool (not a) :: s, trace)
                | _ -> ([], "Panic" :: trace))
    | Lt -> (match top_two_ints stack with
                | Some (i, j) -> (Bool (i < j) :: list_tail (list_tail stack), trace)
                | None -> ([], "Panic" :: trace))
    | Gt -> (match top_two_ints stack with
                | Some (i, j) -> (Bool (i > j) :: list_tail (list_tail stack), trace)
                | None -> ([], "Panic" :: trace))

(* Interpreter Function *)
let interp (s : string) : string list option =
    let cmds = parse_program s in
    let eval_command_wrapper acc cmd = eval_command cmd acc in
    let final_state = list_foldleft cmds ([], []) eval_command_wrapper in
    match final_state with
    | (_, trace) -> 
        if list_contains trace "Panic" then None
        else Some (list_reverse trace)
