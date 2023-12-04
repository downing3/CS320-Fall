#use "./../../../classlib/OCaml/MyOCaml.ml";;

type symbol = 
   | Chr of char 
   | Dig of symbol * char

(* Abstract syntax tree for constants *)
type const =
  | Int of int
  | Bool of bool
  | Unit
  | Symbol of symbol

and coms = com list

(* Type definition for values *)
and value =
  | Const of const
  | Closure of closure

and environment = (string * value) list

and closure = {
   name: string;
   environ : environment;
   body: coms;
}

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

type eval_err =
  | StackUnderflow
  | NonSymbolArgumentForBind
  | NonClosureArgumentForBind
  | UnboundSymbol of string
  | NonSymbolValueForBind
  | NonClosureArgumentForCall
  | NonClosureValueForReturn
  | Panic

exception EvalError of eval_err

type call_stack = (environment * coms) list


(* Helper function to convert a list of characters to a string *)
let rec string_of_chars chars =
  match chars with
  | [] -> ""
  | c :: cs -> String.make 1 c ^ string_of_chars cs

let rec custom_assoc x env =
  match env with
  | [] -> None
  | (sym, value) :: rest ->
    if sym = x then Some value else custom_assoc x rest

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

(*ALL PARSERS*)

(* Parser for a single character *)
let parse_char =
  satisfy (fun c -> ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))

(* Parser for a digit character *)
let parse_digit_char =
  satisfy (fun c -> '0' <= c && c <= '9')

(* Parser for a symbol *)
let parse_symbol =
  let rec build_sym acc =
    (parse_char >>= fun c -> build_sym (Dig (acc, c))) <|>
    (parse_digit_char >>= fun d -> build_sym (Dig (acc, d))) <|>
    pure acc
  in
  parse_char >>= fun c -> build_sym (Chr c) >>= fun sym -> pure (Symbol sym)

(* Parser for constants, with symbol parsing attempted last *)
let parse_const =
  parse_int <|>
  parse_bool <|>
  parse_unit <|>
  parse_symbol  (* Ensure symbol parsing is the last option *)

(* Parser for commands *)
let rec parse_com () =
  let* _ = pure () in  
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
    (keyword "If" >> (whitespaces >> parse_coms ()) >>= fun if_branch ->
      keyword "Else" >> (whitespaces >> parse_coms ()) >>= fun else_branch ->
      keyword "End" >> pure (If (if_branch, else_branch)));
    (keyword "Bind" >> pure Bind);
    (keyword "Lookup" >> pure Lookup);
    (keyword "Fun" >> (whitespaces >> parse_coms ()) >>= fun body ->
      keyword "End" >> pure (Fun body));
    (keyword "Call" >> pure Call);
    (keyword "Return" >> pure Return)
  ]


(* Parser for a sequence of commands *)
and parse_coms () =
  let* _ = pure () in  
  many (parse_com () << keyword ";")


(* interpreter *)

type stack = value list
type trace = string list
type prog = coms

let rec str_of_nat (n : int) : string =
  let d = n mod 10 in 
  let n0 = n / 10 in
  let s = str (chr (d + ord '0')) in 
  if 0 < n0 then
    string_append (str_of_nat n0) s
  else s

let str_of_int (n : int) : string = 
  if n < 0 then
    string_append "-" (str_of_nat (-n))
  else str_of_nat n

let rec string_of_symbol sym =
  match sym with
  | Chr c -> String.make 1 c
  | Dig (s, c) -> string_of_symbol s ^ String.make 1 c

let rec toString c =
  match c with
  | Int i -> str_of_int i
  | Bool b -> if b then "True" else "False"
  | Unit -> "Unit"
  | Symbol s -> string_of_symbol s

let rec retrieve sym env =
  match env with
  | [] -> None
  | (key, value) :: rest ->
    if key = sym then Some value else retrieve sym rest

let rec eval (s : stack) (t : trace) (v : environment) (p : prog) : trace =
  match p with
  | [] -> t
  | Push c :: p0 -> eval (Const c :: s) t v p0
  | Pop :: p0 ->
    (match s with
    | _ :: s0 -> eval s0 t v p0
    | [] -> eval [] ("Panic" :: t) v [])
  | Swap :: p0 ->
    (match s with
    | c1 :: c2 :: s0 -> eval (c2 :: c1 :: s0) t v p0
    | _ -> eval [] ("Panic" :: t) v [])
  | Trace :: p0 ->
    (match s with
    | Const c :: s0 -> eval (Const Unit :: s0) (toString c :: t) v p0
    | _ -> eval [] ("Panic" :: t) v [])
  | Add :: p0 ->
    (match s with
    | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Int (i + j)) :: s0) t v p0
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
  | Bind :: p0 ->
    (match s with
    | Const (Symbol x) :: v0 :: s0 ->
        let sym_str = string_of_symbol x in
        let new_env = (sym_str, v0) :: v in
        eval s0 t new_env p0
    | _ -> eval [] ("Panic" :: t) v [])
  | Lookup :: p0 ->
    (match s with
    | Const (Symbol x) :: s0 ->
        (match custom_assoc (string_of_symbol x) v with
        | Some v0 -> eval (v0 :: s0) t v p0
        | None -> eval [] ("Panic" :: t) v [])
    | _ -> eval [] ("Panic" :: t) v [])
  | Fun cmd :: End :: p0 ->
    (match s with
    | Const (Symbol x) :: s0 ->
      let closure = { name = string_of_symbol x; environ = v; body = cmd } in
      eval (Closure closure :: s0) t v p0
    | _ -> eval [] ("Panic" :: t) v [])
  | Call :: p0 ->
    (match s with
    | Const (Symbol f) :: a :: s0 ->
        (match custom_assoc (string_of_symbol f) v with
        | Some (Closure closure) ->
            let updated_env = (closure.name, Closure closure) :: closure.environ in
            let cc = Symbol (Chr 'c') in
            let new_continuation = Closure { name = ""; environ = v; body = p0 } in
            let new_stack = a :: new_continuation :: s0 in
            eval (new_stack) t updated_env closure.body
        | _ -> eval [] ("Panic" :: t) v [])
    | _ -> eval [] ("Panic" :: t) v [])
  | Return :: p0 ->
    (match s with
    | Const (Symbol f) :: a :: s0 ->
        (match custom_assoc (string_of_symbol f) v with
        | Some (Closure closure) ->
            eval (a :: s0) t closure.environ closure.body
        | _ -> eval [] ("Panic" :: t) v [])
    | _ -> eval [] ("Panic" :: t) v [])



(* putting it all together [input -> parser -> eval -> output] *)

let interp (s : string) : string list option =
  match string_parse (whitespaces >> parse_coms()) s with
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