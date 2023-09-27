#use "./../../../../classlib/OCaml/MyOCaml.ml";;


(* ****** ****** *)

(*
//
Assign2-3: 10 points
//
Please implement foldleft_to_iforeach that turns a
foldleft-loop into an iforeach-loop:
let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = ...
*)
type
('xs, 'x0) foreach = 'xs -> ('x0 -> unit) -> unit

type
('xs, 'x0) rforeach = 'xs -> ('x0 -> unit) -> unit

type
('xs, 'x0) iforeach = 'xs -> (int -> 'x0 -> unit) -> unit

type
('xs, 'x0, 'r0) foldleft = 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0

let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =  
  fun xs fopr ->
    let _ = foldleft xs 0 (fun idx x -> 
      fopr idx x; 
      idx + 1
    )
    in ()
;;








