#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)

let rec sexpr_to_string e = 
  match e with
  | SInt n -> string_of_int n  
  | SAdd exprs -> string_append (string_append "(add " (sexpr_list_to_string exprs)) ")"
  | SMul exprs -> string_append (string_append "(mul " (sexpr_list_to_string exprs)) ")"

and sexpr_list_to_string exprs =
  match exprs with
  | [] -> ""
  | [e] -> sexpr_to_string e
  | e :: es -> string_append (string_append (sexpr_to_string e) " ") (sexpr_list_to_string es)

and parse_sint =
  natural >>= fun n -> pure (SInt n)

and parse_sexprs () =
  many' (fun () -> whitespaces >> parse_sexpr ())

and parse_sexpr () input =
  match parse_sint input with
  | Some _ as result -> result
  | None -> 
    match parse_sadd () input with
    | Some _ as result -> result
    | None -> parse_smul () input

and parse_sadd () input =
  match keyword "(add" input with
  | None -> None
  | Some (_, rest) ->
    match parse_sexprs () rest with
    | None -> None
    | Some (exprs, rest) ->
      match keyword ")" rest with
      | None -> None
      | Some (_, rest) -> Some (SAdd exprs, rest)

and parse_smul () input =
  match keyword "(mul" input with
  | None -> None
  | Some (_, rest) ->
    match parse_sexprs () rest with
    | None -> None
    | Some (exprs, rest) ->
      match keyword ")" rest with
      | None -> None
      | Some (_, rest) -> Some (SMul exprs, rest)

let sexpr_parse s =
  match string_parse (parse_sexpr ()) s with
  | Some (sexpr, []) -> Some sexpr
  | _ -> None
