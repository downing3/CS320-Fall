(*
Assign3-1:
HX-2023-09-26: 10 points
A matrix can be represented as a list of lists.
For instance, [[1;2;3];[4;5;6];[7;8;9]] represents
the following 3x3 square matrix:
1 2 3
4 5 6
7 8 9
Please implement matrix_transpose that returns
the transpose of a given matrix:
let rec
matrix_transpose(xss: 'a list list): 'a list list
For instance, the transpose of the above matrix
is given as follows:
1 4 7
2 5 8
3 6 9
You are allowed to define recursive functions to
solve this problem.
*)
(*[1,2,3];[4,5,6];[7,8,9]*)
(*[1,4,7];[2,5,8];[3,6,9]*)

(*extract the first element of each list*)
let rec head xs = 
  match xs with
  | [] -> []
  | y::ys -> (match y with 
              | [] -> [] 
              | h::t -> h :: head ys)

(*extract the rest of each list*)
let rec tail xs =
  match xs with
  | [] -> []
  | y::ys -> (match y with
              | [] -> [] 
              | h::t -> t :: tail ys)


let rec matrix_transpose(xss: 'a list list): 'a list list = 
  match head xss with
  | [] -> []
  | _ -> head xss :: matrix_transpose (tail xss)
;;
  
  
  





