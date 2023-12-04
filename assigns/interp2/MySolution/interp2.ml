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
  | Bind of string
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
    (keyword "If" >> parse_coms () >>= fun if_branch ->
      keyword "Else" >> parse_coms () >>= fun else_branch ->
      keyword "End" >> pure (If (if_branch, else_branch)));
    (keyword "Bind" >>
      (parse_symbol >>= function
      | Symbol sym -> pure (Bind sym)
      | _ -> failwith "Expected a symbol after 'Bind'"));
    (keyword "Lookup" >> pure Lookup);
    (keyword "Fun" >> parse_coms () >>= fun body ->
      keyword "End" >> pure (Fun body));
    (keyword "Call" >> pure Call);
    (keyword "Return" >> pure Return)
  ]

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

let rec toString c =
  match c with
  | Int i -> str_of_int i
  | Bool b -> if b then "True" else "False"  
  | Unit -> "Unit"
  | Symbol s -> s

and string_of_value v =
  match v with
  | Const c -> toString c
  | Closure (f, _, _) -> "Closure (" ^ f ^ ")"

let is_closure = function
  | Closure _ -> true
  | _ -> false


let rec eval (s : stack) (t : trace) (env : environment) (p : prog) : trace =
  match p with
  | [] -> t
  | Push c :: p0 -> 
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
  | If (if_branch, else_branch) :: p0 ->
    (match s with
    | Const (Bool b) :: s0 -> 
        let branch_to_execute = if b then if_branch else else_branch in
        eval s0 t env (branch_to_execute @ p0)
    | _ -> 
        eval [] ("Panic" :: t) env [])
  | Else :: p0 ->
    eval s t env p0
  | End :: p0 ->
    eval s t env p0
  | Bind x :: p0 ->
    (match s with
    | Const (Symbol sym) :: v :: s0 when sym = x -> 
        let new_env = (sym, v) :: env in
        eval s0 t new_env p0
    | [] | [_] ->  
        eval [] ("Panic" :: t) env []
    | _ ->  
       eval [] ("Panic" :: t) env [])
  | Lookup :: p0 ->
    (match s with
    | Const (Symbol x) :: s0 ->
        (match List.assoc_opt x env with
        | Some closure -> eval (closure :: s0) t env p0
        | None -> 
            raise (EvalError (UnboundSymbol x)))
    | _ -> 
        raise (EvalError NonSymbolArgumentForBind))
  | Fun body :: p0 ->
    let closure = Closure ("", env, body) in 
    eval (closure :: s) t env p0
  | Call :: p0 ->
    (match s with
    | Closure (f, closure_env, closure_commands) :: a :: s0 -> 
        let new_env = (f, Closure (f, closure_env, closure_commands)) :: closure_env in
        let current_continuation = Closure ("cc", env, p0) in
        let new_stack = a :: current_continuation :: s0 in
        eval new_stack t new_env closure_commands 
    | _ ->
        raise (EvalError NonClosureArgumentForCall))
  | Return :: p0 ->
    (match s with
    | Closure (_, closure_env, closure_commands) :: v :: s0 ->
        eval (v :: s0) t closure_env closure_commands
    | _ -> eval [] ("Panic" :: t) env [])
  

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