(* ****** ****** *)

#use "./../../../classlib/OCaml/MyOCaml.ml";;


(* GRAMMAR *)

(* 
grammar: constants 

⟨digit⟩ ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
⟨nat⟩ ::= ⟨digit⟩ | ⟨digit⟩⟨nat⟩
⟨int⟩ ::= ⟨nat⟩ | -⟨nat⟩
⟨bool⟩ ::= True | False
⟨char⟩ ::= a | b | ... | z
⟨sym⟩ ::= ⟨char⟩ | ⟨sym⟩⟨char⟩ | ⟨sym⟩⟨digit⟩
⟨const⟩ ::= ⟨int⟩ | ⟨bool⟩ | Unit | ⟨sym⟩
*)

(* 
grammar: programs 

⟨prog⟩ ::= ⟨coms⟩
⟨com⟩ ::= Push ⟨const⟩ | Pop | Swap | Trace
          | Add | Sub | Mul | Div
          | And | Or | Not
          | Lt | Gt
          | If ⟨coms⟩ Else ⟨coms⟩ End
          | Bind | Lookup
          | Fun ⟨coms⟩ End | Call | Return
⟨coms⟩ ::= ϵ | ⟨com⟩; ⟨coms⟩
*)


(* TYPE DEFINITIONS *)

type sym = 
   | Char of char 
   | CDig of sym * char

type const =
   | Int of int
   | Bool of bool
   | Unit
   | Sym of sym

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

and venv = (string * value) list

and closure = {
   name: sym;
   capt_env: venv;
   body: coms;
}

type stack = value list 

type trace = string list 

type prog = coms


(* HELPER FUNCTIONS *)

let is_lower_case c =
   'a' <= c && c <= 'z'

let is_upper_case c =
   'A' <= c && c <= 'Z'

let is_alpha c =
   is_lower_case c || is_upper_case c

let is_digit c =
   '0' <= c && c <= '9'

let rec fetch (s: string) (v: venv) =
   match v with
   | (x, v0) :: v1 -> 
      if s = x then 
         v0 
      else 
         fetch s v1


(* STRING FUNCTIONS *)

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

let rec str_of_sym(s: sym): string =
   match s with 
   | Char c -> str c
   | CDig (s1, c1) -> string_append (str_of_sym s1) (str c1)

let toString(c: const): string =
   match c with 
   | Int i -> str_of_int i
   | Bool true -> "True"
   | Bool false -> "False"
   | Unit -> "Unit"
   | Sym x -> str_of_sym x


(* PARSERS *)

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
   satisfy is_digit

let rec sym_parser =
  let char_sym_parser =
    char_parser >>= fun c -> pure (Char c)
  in

  let rec concat_sym_parser acc =
    (char_parser >>= fun c ->
      concat_sym_parser (CDig (acc, c)))
    <|>
    (digit_parser >>= fun d ->
      concat_sym_parser (CDig (acc, d)))
    <|>
    pure acc
  in

  char_sym_parser >>= concat_sym_parser


let const_parser =
   int_parser
   <|>
   bool_parser
   <|>
   unit_parser
   <|>
   (sym_parser >>= fun s ->
      pure (Sym s))

let rec com_parser input =
   ((keyword "Push" >> const_parser >>= fun c ->
      pure (Push c))
   <|>
   (keyword "Pop" >> pure Pop)
   <|>
   (keyword "Swap" >> pure Swap)
   <|>
   (keyword "Trace" >> pure Trace)
   <|>
   (keyword "Add" >> pure Add)
   <|>
   (keyword "Sub" >> pure Sub)
   <|>
   (keyword "Mul" >> pure Mul)
   <|>
   (keyword "Div" >> pure Div)
   <|>
   (keyword "And" >> pure And)
   <|>
   (keyword "Or" >> pure Or)
   <|>
   (keyword "Not" >> pure Not)
   <|>
   (keyword "Lt" >> pure Lt)
   <|>
   (keyword "Gt" >> pure Gt)
   <|>
   (keyword "If" >> coms_parser >>= fun c1 ->
      keyword "Else" >> coms_parser >>= fun c2 ->
      keyword "End" >> pure (If (c1, c2)))
   <|> 
   (keyword "Bind" >> pure Bind)
   <|>
   (keyword "Lookup" >> pure Lookup)
   <|>
   (keyword "Fun" >> coms_parser >>= fun c ->
      keyword "End" >> pure (Fun c))
   <|>
   (keyword "Call" >> pure Call)
   <|>
   (keyword "Return" >> pure Return)) input

and coms_parser input =   
   (many (com_parser << keyword ";")) input


(* INTERPRETER FUNCTIONS *)


let rec eval_step(s: stack)(t: trace)(v: venv)(p: prog): trace =
   match p with
   | [] -> t
   | Push c :: p0 -> eval_step (Const c :: s) t v p0
   | Pop :: p0 -> 
      (match s with 
      | _ :: s0 -> eval_step s0 t v p0
      | [] -> eval_step [] ("Panic" :: t) v [])
   | Swap :: p0 -> 
      (match s with 
      | c1 :: c2 :: s0 -> eval_step (c2 :: c1 :: s0) t v p0
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
   | Trace :: p0 -> 
      (match s with
      | Const c :: s0 -> eval_step (Const Unit :: s0) (toString c :: t) v p0
      | [] -> eval_step [] ("Panic" :: t) v [])
   | Add :: p0 -> 
      (match s with
      | Const(Int i) :: Const(Int j) :: s0 -> eval_step (Const(Int (i + j)) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
   | Sub :: p0 -> 
      (match s with
      | Const(Int i) :: Const(Int j) :: s0 -> eval_step (Const(Int (i - j)) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
  | Mul :: p0 -> 
      (match s with
      | Const(Int i) :: Const(Int j) :: s0 -> eval_step (Const(Int (i * j)) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
  | Div :: p0 -> 
      (match s with
      | Const(Int i) :: Const(Int 0) :: s0 -> eval_step [] ("Panic" :: t) v []
      | Const(Int i) :: Const(Int j) :: s0 -> eval_step (Const(Int (i / j)) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
   | And :: p0 -> 
      (match s with
      | Const(Bool a) :: Const(Bool b) :: s0 -> eval_step (Const(Bool (a && b)) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
  | Or :: p0 -> 
      (match s with
      | Const(Bool a) :: Const(Bool b) :: s0 -> eval_step (Const(Bool (a || b)) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
   | Not :: p0 -> 
      (match s with
      | Const(Bool a) :: s0 -> eval_step (Const(Bool (not a)) :: s0) t v p0
      | _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
   | Lt :: p0 -> 
      (match s with
      | Const(Int i) :: Const(Int j) :: s0 -> eval_step (Const(Bool (i < j)) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
  | Gt :: p0 -> 
      (match s with
      | Const (Int i) :: Const (Int j) :: s0 -> eval_step (Const(Bool (i > j)) :: s0) t v p0
      | _ :: _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v []
      | _ :: [] -> eval_step [] ("Panic" :: t) v [])
   | If (c1, c2) :: p0 -> 
      (match s with 
      | Const(Bool b) :: s0 -> 
         if b = true then 
            eval_step s0 t v (c1 @ p0)
         else 
            eval_step s0 t v (c2 @ p0)
      | _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v [])
   | Bind :: p0 -> 
      (match s with 
      | Const (Sym x) :: v0 :: s0 -> 
         eval_step s0 t ((str_of_sym x, v0) :: v) p0 
      | _ :: v0 :: s0 -> eval_step [] ("Panic" :: t) v []
      | _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v [])
   | Lookup :: p0 ->
      (match s with 
      | Const (Sym x) :: s0 -> 
         let found = fetch (str_of_sym x) v in 
         eval_step (found :: s0) t v p0
         (*
         (match v with 
         | (x, v0) :: v1 -> eval_step (v0 :: s0) t v p0
         | _ :: v0 -> eval_step [] ("Panic" :: t) v [])*)
      | _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v [])
   | Fun c :: p0 ->
      (match s with 
      | Const (Sym x) :: s0 -> 
         let f = {name = x; capt_env = v; body = c} in 
         eval_step (Closure f :: s0) t v p0
      | _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v [])
   | Call :: p0 -> 
      (match s with
      | Const (Sym f) :: a :: s0 ->
        let closure = fetch (str_of_sym f) v in 
        eval_step (a :: Const (Sym f) :: s0) t ((str_of_sym f, closure) :: v) p0
      | _ :: a :: s0 -> eval_step [] ("Panic" :: t) v []
      | _ :: s0 -> eval_step [] ("Panic" :: t) v []
      | [] -> eval_step [] ("Panic" :: t) v [])
    | Return :: p0 -> 
        (match s with 
        | Const (Sym f) :: a :: s0 ->
            (match fetch (str_of_sym f) v with
            | Closure closure ->
                eval_step (a :: s0) t closure.capt_env closure.body
            | _ -> eval_step [] ("Panic" :: t) v [])
        | _ :: a :: s0 -> eval_step [] ("Panic" :: t) v []
        | _ :: s0 -> eval_step [] ("Panic" :: t) v []
        | [] -> eval_step [] ("Panic" :: t) v [])

let interp (s: string): string list option =
   match string_parse (whitespaces >> coms_parser) s with
  | Some (p, []) -> Some (eval_step [] [] [] p)
  | _ -> None
