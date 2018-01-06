(*Lists*)
let rec last_element x = match x with
 | h::[] -> h
 | h::t -> last_element t

let rec list_minus l1 l2 = match l1 with
 | h::t -> 
  let rec check num x = match x with
   | h1::t1 -> if num = h1 then false else check num t1
   | [] -> true
  in
  if (check h l2) = true then h::(list_minus t l2) else list_minus t l2
 | [] -> []

let rec map_nth f l1 n = 
 let rec _map f1 r1 x = match r1 with 
  | [] -> []
  | h::t -> if x=1 then f h::(_map f1 t n) else h::(_map f1 t (x-1))
 in
 _map f l1 n

(*N-base arithmetic*)

let rec check_nbase n l = match l with
 | [] -> true
 | h::t -> if h/n = 0 then check_nbase n t else false

(*subtract the l2 from the l1 
 in case l1 is greater than l2 the result will be wrong*)
let rec subtract_nbase n l1 l2 = match l1 with
 h1::t1 -> ( match l2 with
  | h2::i2::t2-> if h2>h1 then h1+n-h2::(subtract_nbase n t1 (i2+1::t2)) else h1-h2::(subtract_nbase n t1 (i2::t2))
  | h2::t2 -> if h2>h1 then h1+n-h2::(subtract_nbase n t1 t2) else h1-h2::(subtract_nbase n t1 t2)
  | [] -> h1::t1 )
 | [] -> []

(*helper functions for the multiplication*)
(*add 2 n-base numbers *)
let rec add n l1 l2 = match l2 with 
 | h1::t1 -> (match l1 with
  | h2::i2::t2 -> if h2+h1>=n then ((h1+h2) mod n)::(add n t1 (i2+1::t2)) else h1+h2::(add n t1 (i2::t2))
  | h2::[] -> if h2+h1>=n && t1 <> [] then ((h1+h2) mod n)::(add n t1 [1]) else if h2+h1>=n then ((h1+h2) mod n)::(add n (t1 @ [1]) []) else h1+h2::(add n t1 [])
  | [] -> h1::t1)
 | [] -> l1

(*append n zeros to a list*)
let rec zer n lst = match n with
 | 0 -> lst
 | _ -> zer (n-1) (0::lst)

(*multiply a list with a number*)
let rec num_mul base lst num krat = match lst with
 | h::t -> ((h*num+krat) mod base)::(num_mul base t num (h*num/base))
 | [] -> if krat <> 0 then krat::[] else []

(*multiply two nbase numbers*)
let multiply_nbase base lst1 lst2 = 
 let rec mul bs l1 l2 iter = match l2 with
  | h::t -> let ret = num_mul bs l1 h 0 in 
   let zer_ret = zer iter ret in 
   add bs zer_ret (mul bs l1 t (iter+1))
  | [] -> []
 in
 mul base lst1 lst2 0


(*Bolean syntax trees*)

(*Redefining the type of AST*)

type bool_ast =
    True
  | False
  | And of bool_ast * bool_ast
  | Or of bool_ast * bool_ast
  | Not of bool_ast

let rec eval_ast stmt = match stmt with
 | True -> true
 | False -> false
 | And(x,y) -> (eval_ast x ) && (eval_ast y)
 | Or(x,y) -> (eval_ast x) || (eval_ast y)
 | Not(x) -> not (eval_ast x)

let rec has_identity stmt = match stmt with
 | True -> false
 | False -> false
 | And(x,y) -> if x=y then true else (has_identity x) && (has_identity y)
 | Or(x,y) -> (has_identity x) || (has_identity y)
 | Not x -> has_identity x

let rec rewrite_demorgan x = match x with
 | True -> True
 | False -> False
 | Not(And(z,y)) -> Or(Not(rewrite_demorgan z), Not(rewrite_demorgan y))
 | And(z,y) -> And(rewrite_demorgan z, rewrite_demorgan y)
 | Or(z,y) -> Or(rewrite_demorgan z, rewrite_demorgan y)
 | Not y -> Not(rewrite_demorgan y) 











