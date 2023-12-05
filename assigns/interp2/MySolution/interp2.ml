#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* type definitions for symbols and constants *)
type symbol = 
   | Char of char 
   | CDig of symbol * char

type const =
   | Int of int
   | Bool of bool
   | Unit
   | Sym of symbol

(* type definitions for commands and composite structures *)
type com =
   | Push of const | Pop | Swap | Trace
   | Add | Sub | Mul | Div
   | And | Or | Not 
   | Lt | Gt
   | If of coms * coms
   | Bind | Lookup 
   | Fun of coms | Call | Return

and coms = com list

type value =
    | Const of const 
    | Closure of closure

type stack = value list 
type trace = string list 
type prog = coms

and enviornment = (string * value) list
and closure = symbol * enviornment * coms

(* ------------------------------------------------------------ *)

(* helper functions for character checks and string conversions *)
let uppercase c =
  'A' <= c && c <= 'Z'
let lowercase c =
   'a' <= c && c <= 'z'
let is_alpha c =
  lowercase c || uppercase c
let is_dig c =
   '0' <= c && c <= '9'

(* environment lookup function *)
let rec lookup_env (name: string) (env: enviornment) : value =
  match env with
  | (x, v_value) :: rest ->
    if name = x then v_value 
    else lookup_env name rest
  | [] -> failwith "Variable not found in environment"

(* string conversion functions *)
let rec str_of_nat(n: int): string = 
   let d = n mod 10 in 
   let n0 = n / 10 in
   let s = str (chr(d + ord '0')) in 
   if 0 < n0 then
      string_append(str_of_nat n0) s
   else 
      s

let str_of_int(n: int): string = 
   if n < 0 then
      string_append "-" (str_of_nat (-n))
   else 
      str_of_nat n

let rec str_of_sym(s: symbol): string =
   match s with 
   | Char c -> str c
   | CDig (s1, c1) -> string_append (str_of_sym s1) (str c1)

(* convert constant to string representation *)
let toString(c: const): string =
   match c with 
   | Int i -> str_of_int i
   | Bool true -> "True"
   | Bool false -> "False"
   | Unit -> "Unit"
   | Sym x -> str_of_sym x

  (* ------------------------------------------------------------ *)


(* parsers for different types of constants *)
let nat_parser = 
   let* n = natural << whitespaces in 
      pure n

let int_parser =
   (let* n = nat_parser in 
      pure (Int n))
   <|>
   (keyword "-" >> let* n = nat_parser in 
      pure (Int (-n)))

let bool_parser =
   (keyword "True" >> pure (Bool true)) 
   <|> 
   (keyword "False" >> pure (Bool false))

let unit_parser =
   keyword "Unit" >> pure Unit

let char_parser =
   satisfy is_alpha

let digit_parser = 
   satisfy is_dig

let rec sym_parser =
  let char_sym_parser =
    char_parser >>= fun c -> pure (Char c)
  in

  let rec combine_symbol_parsers acc =
    (char_parser >>= fun c ->
      combine_symbol_parsers (CDig (acc, c))) <|>
    (digit_parser >>= fun d ->
      combine_symbol_parsers (CDig (acc, d)))
    <|>
    pure acc
  in

  char_sym_parser >>= combine_symbol_parsers


let const_parser =
   int_parser <|>
   bool_parser <|>
   unit_parser <|>
   (sym_parser >>= fun s ->
      pure (Sym s))

let rec com_parser input =
   ((keyword "Push" >> const_parser >>= fun c ->
      pure (Push c)) <|>
   (keyword "Pop" >> pure Pop) <|>
   (keyword "Swap" >> pure Swap) <|>
   (keyword "Trace" >> pure Trace) <|>
   (keyword "Add" >> pure Add) <|>
   (keyword "Sub" >> pure Sub) <|>
   (keyword "Mul" >> pure Mul) <|>
   (keyword "Div" >> pure Div) <|>
   (keyword "And" >> pure And) <|>
   (keyword "Or" >> pure Or) <|>
   (keyword "Not" >> pure Not) <|>
   (keyword "Lt" >> pure Lt) <|>
   (keyword "Gt" >> pure Gt) <|>
   (keyword "If" >> coms_parser >>= fun c1 ->  (* parse the 'If' keyword followed by a sequence of commands (c1) *)
      keyword "Else" >> coms_parser >>= fun c2 -> (* after 'If', parse the 'Else' keyword followed by another sequence of commands (c2) *)
      keyword "End" >> pure (If (c1, c2))) <|>  (* last, parse the 'End' keyword and construct an 'If' command with the two command sequences (c1 and c2) *)
   (keyword "Bind" >> pure Bind) <|>
   (keyword "Lookup" >> pure Lookup) <|>
   (keyword "Fun" >> coms_parser >>= fun c -> (* parse the 'Fun' keyword followed by a sequence of commands (c) *)
      keyword "End" >> pure (Fun c)) <|>  (* after parsing the commands, parse the 'End' keyword and construct a 'Fun' command with the command sequence (c) *)
   (keyword "Call" >> pure Call) <|>
   (keyword "Return" >> pure Return)) input

and coms_parser input =   
   (many (com_parser << keyword ";")) input


let rec eval (s : stack) (t : trace) (v : enviornment) (p : prog) : trace =
  match p with
  (* termination state returns the trace *)
  | [] -> t
  | Push c :: p0 -> eval (Const c :: s) t v p0 (*push a constant onto th stack*)
  | Pop :: p0 ->
    (match s with
    | _ :: s0 -> eval s0 t v p0
    | [] -> eval [] ("Panic" :: t) v []) (*if stack is empty, panic*)
  | Swap :: p0 -> (*swap top two elemnts of the stack*)
    (match s with
    | c1 :: c2 :: s0 -> eval (c2 :: c1 :: s0) t v p0
    | _ -> eval [] ("Panic" :: t) v []) (*panic if less than two elements*)
  | Trace :: p0 ->
    (match s with
    | Const c :: s0 -> eval (Const Unit :: s0) (toString c :: t) v p0 (*trace top element of stack*)
    | _ -> eval [] ("Panic" :: t) v []) (*panic if empty stack*)
  | Add :: p0 ->
    (match s with
    | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Int (i + j)) :: s0) t v p0 (*these are all pretty self explanatory and are just edits of the class solution from part1*)
    | _ -> eval [] ("Panic" :: t) v [])
  | Sub :: p0 ->
    (match s with
    | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Int (i - j)) :: s0) t v p0
    | _ -> eval [] ("Panic" :: t) v [])
  | Mul :: p0 ->
    (match s with
    | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Int (i * j)) :: s0) t v p0
    | _ -> eval [] ("Panic" :: t) v [])
  | Div :: p0 ->
    (match s with
    | Const (Int i) :: Const (Int 0) :: s0 -> eval [] ("Panic" :: t) v []
    | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Int (i / j)) :: s0) t v p0
    | _ -> eval [] ("Panic" :: t) v [])
  | And :: p0 ->
    (match s with
    | Const (Bool a) :: Const (Bool b) :: s0 -> eval (Const (Bool (a && b)) :: s0) t v p0
    | _ -> eval [] ("Panic" :: t) v [])
  | Or :: p0 ->
    (match s with
    | Const (Bool a) :: Const (Bool b) :: s0 -> eval (Const (Bool (a || b)) :: s0) t v p0
    | _ -> eval [] ("Panic" :: t) v [])
  | Not :: p0 ->
    (match s with
    | Const (Bool a) :: s0 -> eval (Const (Bool (not a)) :: s0) t v p0
    | _ -> eval [] ("Panic" :: t) v [])
  | Lt :: p0 ->
    (match s with
    | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Bool (i < j)) :: s0) t v p0
    | _ -> eval [] ("Panic" :: t) v [])
  | Gt :: p0 ->
    (match s with
    | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Bool (i > j)) :: s0) t v p0
    | _ -> eval [] ("Panic" :: t) v [])
  | If (c1, c2) :: p0 ->
    (match s with
    | Const (Bool b) :: s0 ->
        let branch = if b then c1 else c2 in
        eval s0 t v (branch @ p0)
    | _ -> eval [] ("Panic" :: t) v [])
   | Bind :: p0 ->  (*bind a symbol to a value*)
  (match s with 
  | Const (Sym x) :: value :: s0 -> 
    eval s0 t ((str_of_sym x, value) :: v) p0 
  | _ :: value :: s0 -> eval [] ("Panic" :: t) v []
  | _ :: s0 -> eval [] ("Panic" :: t) v []
  | [] -> eval [] ("Panic" :: t) v [])
  | Lookup :: p0 ->
    (match s with 
    | Const (Sym x) :: s0 -> 
      let found = lookup_env (str_of_sym x) v in 
      eval (found :: s0) t v p0
    | _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v [])

  | Fun c :: p0 ->
    (match s with 
    | Const (Sym x) :: s0 -> 
      eval (Closure (x, v, c) :: s0) t v p0 (*create new function closure*)
    | _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v [])

  | Call :: p0 -> 
    (match s with
    | Closure (f, vf, c) :: a :: s0 ->
      eval (a :: Closure (f, v, p0) :: s0) t ((str_of_sym f, Closure (f, vf, c)) :: vf) c (*calling  a fucntion closure*)
    | _ :: a :: s0 -> eval [] ("Panic" :: t) v []
    | _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v [])

  | Return :: p0 -> 
    (match s with 
    | Closure (f, vf, c) :: a :: s0 ->
      eval (a :: s0) t vf c (*return a fucntion call*)
    | _ :: a :: s0 -> eval [] ("Panic" :: t) v []
    | _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v [])

(* ------------------------------------------------------------ *)

(* putting it all together [input -> parser -> eval -> output] *)

let interp (s: string): string list option =
   match string_parse (whitespaces >> coms_parser) s with
  | Some (p, []) -> Some (eval [] [] [] p)
  | _ -> None

(* ------------------------------------------------------------ *)

(* interp from file *)

let read_file (fname: string): string =
   let fp = open_in fname in
   let s = string_make_fwork (fun work ->
      try
         while true do
            work (input_char fp)
         done
      with _ -> ())
   in
   close_in fp; s

let interp_file (fname: string): string list option =
   let src = read_file fname in
   interp src