#use "./../../../classlib/OCaml/MyOCaml.ml";;

(* Abstract syntax tree for constants *)
type const =
  | Int of int
  | Bool of bool
  | Unit
  | Symbol of string

and coms = com list

(* Type definition for values *)
and value =
  | Const of const
  | Closure of string * environment * coms

and environment = (string * value) list

(* Extended command types *)
and com = 
  | Push of const
  | Pop
  | Swap
  | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt
  | If of coms * coms
  | Else
  | End
  | Bind
  | Lookup
  | Fun of coms
  | Call
  | Return

(* Helper function to convert a list of characters to a string *)
let rec string_of_chars chars =
  match chars with
  | [] -> ""
  | c :: cs -> String.make 1 c ^ string_of_chars cs

let rec choice parsers input =
  match parsers with
  | [] -> None
  | parser :: rest -> 
      match parser input with
      | Some _ as result -> result
      | None -> choice rest input

(* parsers for interp1 *)
let parse_nat = 
  let* n = natural << whitespaces in pure n

let parse_int =
  (let* n = parse_nat in pure (Int n)) <|>
  (keyword "-" >> let* n = parse_nat in pure (Int (-n)))

let parse_bool =
  (keyword "True" >> pure (Bool true)) <|>
  (keyword "False" >> pure (Bool false))

let parse_unit =
  keyword "Unit" >> pure Unit

(* Parser for a single character *)
let parse_char =
  satisfy (fun c -> ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))

(* Parser for a digit character *)
let parse_digit_char =
  satisfy (fun c -> '0' <= c && c <= '9')

(* Parser for a symbol *)
let parse_symbol =
  let* first_char = parse_char in
  let* rest_chars = many (parse_char <|> parse_digit_char) << whitespaces in
  pure (Symbol (String.make 1 first_char ^ string_of_chars rest_chars))

let parse_const =
  parse_int <|>
  parse_bool <|>
  parse_unit <|>
  parse_symbol

let rec parse_com () =
  choice [
    (keyword "Push" >> parse_const >>= fun c -> pure (Push c));
    (keyword "Pop" >> pure Pop);
    (keyword "Swap" >> pure Swap);
    (keyword "Trace" >> pure Trace);
    (keyword "Add" >> pure Add);
    (keyword "Sub" >> pure Sub);
    (keyword "Mul" >> pure Mul);
    (keyword "Div" >> pure Div);
    (keyword "And" >> pure And);
    (keyword "Or" >> pure Or);
    (keyword "Not" >> pure Not);
    (keyword "Lt" >> pure Lt);
    (keyword "Gt" >> pure Gt);
    (keyword "Else" >> pure Else);
    (keyword "End" >> pure End);
    (keyword "Bind" >> pure Bind);
    (keyword "Lookup" >> pure Lookup);
    (keyword "Call" >> pure Call);
    (keyword "Return" >> pure Return)
  ]

and parse_coms () = many (parse_com () << keyword ";")

(* interpreter *)

type stack = value list
type trace = string list
type prog = coms

let rec str_of_nat (n : int) : string =
  let d = n mod 10 in 
  let n0 = n / 10 in
  let s = String.make 1 (Char.chr (d + int_of_char '0')) in 
  if 0 < n0 then
    s ^ str_of_nat n0
  else s

let str_of_int (n : int) : string = 
  if n < 0 then
    "-" ^ str_of_nat (-n)
  else str_of_nat n

let rec toString c =
  match c with
  | Int i -> str_of_int i
  | Bool b -> string_of_bool b
  | Unit -> "Unit"
  | Symbol s -> s

and string_of_value v =
  match v with
  | Const c -> toString c
  | Closure (f, _, _) -> "Closure (" ^ f ^ ")"


let rec eval (s : stack) (t : trace) (env : environment) (p : prog) : trace =
  match p with
  | [] -> t
  | Push c :: p0 -> 
    let () = print_endline "Push" in
    eval (Const c :: s) t env p0
  | Pop :: p0 ->
    (match s with
    | _ :: s0 -> eval s0 t env p0
    | [] -> eval [] ("Panic" :: t) env [])
  | Swap :: p0 ->
    (match s with
    | v1 :: v2 :: s0 -> eval (v2 :: v1 :: s0) t env p0
    | _ -> eval [] ("Panic" :: t) env [])
  | Trace :: p0 ->
    (match s with
    | (Const c) :: s0 -> eval (Const Unit :: s0) (toString c :: t) env p0
    | _ -> eval [] ("Panic" :: t) env [])
  | Add :: p0 ->
    (match s with
    | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Int (i + j)) :: s0) t env p0
    | _ -> eval [] ("Panic" :: t) env [])
  | Sub :: p0 ->
    (match s with
    | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Int (i - j)) :: s0) t env p0
    | _ -> eval [] ("Panic" :: t) env [])
  | Mul :: p0 ->
    (match s with
    | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Int (i * j)) :: s0) t env p0
    | _ -> eval [] ("Panic" :: t) env [])
  | Div :: p0 ->
    (match s with
    | Const (Int i) :: Const (Int 0) :: s0 -> eval [] ("Panic" :: t) env []
    | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Int (i / j)) :: s0) t env p0
    | _ -> eval [] ("Panic" :: t) env [])
  | And :: p0 ->
    (match s with
    | Const (Bool a) :: Const (Bool b) :: s0 -> eval (Const (Bool (a && b)) :: s0) t env p0
    | _ -> eval [] ("Panic" :: t) env [])
  | Or :: p0 ->
    (match s with
    | Const (Bool a) :: Const (Bool b) :: s0 -> eval (Const (Bool (a || b)) :: s0) t env p0
    | _ -> eval [] ("Panic" :: t) env [])
  | Not :: p0 ->
    (match s with
    | Const (Bool a) :: s0 -> eval (Const (Bool (not a)) :: s0) t env p0
    | _ -> eval [] ("Panic" :: t) env [])
  | Lt :: p0 ->
    (match s with
    | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Bool (i < j)) :: s0) t env p0
    | _ -> eval [] ("Panic" :: t) env [])
  | Gt :: p0 ->
    (match s with
    | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Bool (i > j)) :: s0) t env p0
    | _ -> eval [] ("Panic" :: t) env [])

  | If (c1, c2) :: p0 ->
  (match s with
  | Const (Bool b) :: s0 ->
      if b then
        eval s0 t env c1  
      else
        eval s0 t env c2  
  | _ ->
      eval [] ("Panic" :: t) env [])
  | Else :: p0 ->
    eval s t env p0
  | End :: p0 ->
    eval s t env p0

  
  | Bind :: p0 ->
  (match s with
  | (Const (Symbol x)) :: v :: s0 ->
      eval s0 t ((x, v) :: env) p0  
  | _ ->
      eval [] ("Panic" :: t) env [])

| Lookup :: p0 ->
  (match s with
  | Const (Symbol x) :: s0 ->
      let rec lookup_symbol x env =
        match env with
        | [] -> eval [] ("Panic" :: t) env []  
        | (key, valll) :: rest ->
            if key = x then
              match valll with
              | Const cv -> eval (Const cv :: s0) t env p0
              | _ -> eval [] ("Panic" :: t) env []
            else lookup_symbol x rest
      in
      lookup_symbol x env
  | _ -> eval [] ("Panic" :: t) env [])

  | Fun c :: p0 ->
    (match s with
    | (Const (Symbol x)) :: s0 ->
        let closure = Closure (x, env, c) in
        eval (closure :: s0) t env p0  
    | _ ->
        eval [] ("Panic" :: t) env [])  
  
  | Call :: p0 ->
  (match s with
  | a :: Closure (f, closure_env, c_body) :: s0 ->
      let new_env = (f, Closure (f, closure_env, c_body)) :: closure_env in
      eval [a] t new_env c_body  
  | _ ->
      eval [] ("Panic" :: t) env []) 
  
| Return :: p0 ->
  (match s with
  | v :: s0 ->
      (match v with
      | Closure (_, closure_env, _) ->
          eval s0 t closure_env p0 
      | _ ->
          eval [] ("Panic" :: t) env [])
  | _ ->
      eval [] ("Panic" :: t) env [])

  

(* putting it all together [input -> parser -> eval -> output] *)

let interp (s : string) : string list option =
  match string_parse (whitespaces >> parse_coms ()) s with
  | Some (p, []) -> Some (eval [] [] [] p)
  | _ -> None

(* interp from file *)

let read_file (fname : string) : string =
  let fp = open_in fname in
  let s = string_make_fwork (fun work ->
      try
        while true do
          work (input_char fp)
        done
      with _ -> ())
  in
  close_in fp; s

let interp_file (fname : string) : string list option =
  let src = read_file fname in
  interp src