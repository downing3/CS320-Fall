#use "./../../../classlib/OCaml/MyOCaml.ml";;

(* TYPE DEFINITIONS *)
type const = Int of int | Bool of bool | Unit | Invalid
type command = Push of const | Pop | Trace | Add | Sub | Mul | Div | And | Or | Not | Lt | Gt
type stack = const list
type trace = string list

(* CUSTOM HELPER FUNCTIONS *)
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

(* PARSING CONSTS & COMMANDS *)
let parse_const str = 
  if str = "True" then Bool true
  else if str = "False" then Bool false
  else if str = "Unit" then Unit
  else if string_forall str is_digit_or_minus then
     match str with
     | "-" -> Invalid  (* Single "-" is not a valid integer *)
     | _ -> Int (string_to_int str)
  else Invalid  (* Unrecognized constant *)


let parse_command str = 
   let parts = string_split (trim str) ' ' in
   match parts with
   | ["Push"; const_str] -> 
       let const = parse_const const_str in
       if const = Invalid then None
       else Some (Push const)
   | ["Pop"] -> Some Pop
   | ["Trace"] -> Some Trace
   | ["Add"] -> Some Add
   | ["Sub"] -> Some Sub
   | ["Mul"] -> Some Mul
   | ["Div"] -> Some Div
   | ["And"] -> Some And
   | ["Or"] -> Some Or
   | ["Not"] -> Some Not
   | ["Lt"] -> Some Lt
   | ["Gt"] -> Some Gt
   | _ -> None

(* PARSING FUNCTIONS *)
let parse_program program = 
   list_map parse_command (string_split program ';')

let eval_command cmd (stack, trace) = match cmd with
    | Push c -> 
        if c = Invalid then ([], "Panic" :: trace)
        else (c :: stack, trace)
    | Pop -> (match stack with
                | _ :: s -> (s, trace)
                | [] -> ([], "Panic" :: trace))
    | Trace -> (match stack with
            | c :: s -> (Unit :: s, toString c :: trace) 
            | [] -> ([], "Panic" :: trace)) 
    | Add -> (match stack with
            | Int i :: Int j :: s -> (Int (i + j) :: s, trace)
            | Int _ :: _ | _ :: Int _ :: _ | [] -> ([], "Panic" :: trace) 
            | _ :: _ :: _ | _ :: [] -> ([], "Panic" :: trace))  
    | Sub -> (match stack with
            | Int i :: Int j :: s -> (Int (i - j) :: s, trace)  
            | Int _ :: _ | _ :: Int _ :: _ | [] -> ([], "Panic" :: trace) 
            | _ :: _ :: _ | _ :: [] -> ([], "Panic" :: trace)) 
    | Mul -> (match stack with
            | Int i :: Int j :: s -> (Int (i * j) :: s, trace) 
            | Int _ :: _ | _ :: Int _ :: _ | [] -> ([], "Panic" :: trace)
            | _ :: _ :: _ | _ :: [] -> ([], "Panic" :: trace)) 
    | Div -> (match stack with
            | Int i :: Int j :: s when j != 0 -> (Int (j / i) :: s, trace)
            | Int _ :: Int 0 :: _ -> ([], "Panic" :: trace)
            | Int _ :: _ | _ :: Int _ :: _ | [] -> ([], "Panic" :: trace)
            | _ :: _ :: _ | _ :: [] -> ([], "Panic" :: trace))
    | And -> (match stack with
            | Bool a :: Bool b :: s -> (Bool (a && b) :: s, trace)
            | Bool _ :: _ | _ :: Bool _ :: _ | [] -> ([], "Panic" :: trace)
            | _ :: _ :: _ | _ :: [] -> ([], "Panic" :: trace))
    | Or -> (match stack with
            | Bool a :: Bool b :: s -> (Bool (a || b) :: s, trace)
            | Bool _ :: _ | _ :: Bool _ :: _ | [] -> ([], "Panic" :: trace) 
            | _ :: _ :: _ | _ :: [] -> ([], "Panic" :: trace))
    | Not -> (match stack with
            | Bool a :: s -> (Bool (not a) :: s, trace) 
            | [] -> ([], "Panic" :: trace)
            | _ :: _ -> ([], "Panic" :: trace))
    | Lt -> (match stack with
            | Int i :: Int j :: s -> (Bool (i < j) :: s, trace) 
            | Int _ :: _ | _ :: Int _ :: _ | [] -> ([], "Panic" :: trace)
            | _ :: _ :: _ | _ :: [] -> ([], "Panic" :: trace)) 
    | Gt -> (match stack with
            | Int i :: Int j :: s -> (Bool (i > j) :: s, trace) 
            | Int _ :: _ | _ :: Int _ :: _ | [] -> ([], "Panic" :: trace)
            | _ :: _ :: _ | _ :: [] -> ([], "Panic" :: trace))  

(* INTERP FUNCTION *)
let interp (s : string) : string list option =
  let cmds = string_split s ';' in
  let rec eval_commands cmds (stack, trace) =
      match cmds with
      | [] -> (stack, trace)
      | cmd_str :: t -> 
          match parse_command cmd_str with
          | None -> ([], "Panic" :: trace) 
          | Some cmd -> eval_commands t (eval_command cmd (stack, trace))
  in
  let final_state = eval_commands cmds ([], []) in
  match final_state with
  | (_, trace) -> 
      if list_contains trace "Panic" then Some ["Panic"]
      else Some (list_reverse trace)