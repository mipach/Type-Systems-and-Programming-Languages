open Ast

let rec freevars ast = match ast with
| EVar(x) -> x::[]
| EVal(VFun(x,e)) -> freevars e
| EApp(e1,e2) -> (freevars e1)@(freevars e2)

let rec rename ast x y = match ast with
| EVar(x1) -> if x1=x then EVar(y) else EVar(x1)
| EVal(VFun(x1,e)) -> if x1=x then EVal(VFun(y,(rename e x1 y))) else EVal(VFun(x1,(rename e x1 y)))
| EApp(e1,e2) -> EApp((rename e1 x y ),(rename e2 x y))


let rec isvalue ast = match ast with
| EVal(_) -> true
| EApp(e1,e2) -> if (isvalue e1)=true && (isvalue e2)=true then true else false
| _ -> false;;

let next = 
 let counter = ref 0 in
 fun str ->
  let n = !counter in
  incr counter;
  str^(string_of_int n)

(*
let subst e x e'  = 
 let fv = freevar e in 
 let rec helper e = 
  match e with
   | EVar(v) -> let v' = next v in EVar(v')
   | EVal(VFun(v,exp)) -> let v' = next v in EVal(VFun(v',rename (helper exp) v v'))
   | EApp(exp1,exp2) -> EApp(helper exp1,helper exp2) 
 in helper e 
*)
(*checks if a variable is indeed free variable*)
let rec check_list lst value = match lst with
 | h::t -> if h=value then true else check_list t value
 | [] -> false


let subst e x e' =
 let fv = freevars e in
 let rec helper e =
 match e with
   | EVar(v) -> if (check_list fv v ) then e'  else EVar(v)
   | EVal(VFun(v,exp)) -> let v' = next v in EVal(VFun(v',rename (helper exp) v v')) 
   | EApp(exp1,exp2) -> EApp(helper exp1,helper exp2)  
 in helper e


exception Cannot_step1 of Ast.exp
exception Cannot_step2 of Ast.exp
exception Cannot_step of Ast.exp

let rec lazy_step ast = match ast with
| EApp(EVal(VFun(x,e1)),e2 ) -> subst e1 x e2
| EApp(e1,e2) -> EApp(lazy_step e1,e2)
| e -> raise (Cannot_step e)


let rec eager_eval ast = match ast with
| EVal(VFun(x,e)) -> EVal(VFun(x,e))
| EApp(e1,e2) ->( match e2 with
  | EApp(EVal(VFun(x',e1')),e2') -> eager_eval (EApp(e1,(subst e1' x' e2')))
  | EVal(VFun(x',e1')) ->( match e1 with
   | EApp(EVal(VFun(x2,e2')),e3) -> eager_eval (subst e2' x2 e3)
   (*| EVal(VFun(x2,e2)) -> EVal(VFun(x2,e2)) *)
   | EVal(VFun(x22,e22)) -> eager_eval (subst e22 x22 e2 )
   | e -> raise (Cannot_step1 e))
  | e -> raise (Cannot_step2 e))
| e -> raise (Cannot_step e)

