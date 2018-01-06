open Ast

exception Type_error

let rec evalFun tp = match tp with
| TBool -> TBool
| TUnit -> TUnit
| TFun(x1,x2) -> TFun((evalFun x1),(evalFun x2))


let rec lookup (lst:Ast.env) (el:Ast.var) = match lst with
| h::t ->( match h with
  | (x1,x2) -> if x1 = el then( match x2 with 
    | TBool -> TBool
    | TUnit -> TUnit
    | TFun(arg1,arg2) -> TFun(evalFun arg1, evalFun arg2) )else lookup t el
  | _ -> raise Type_error)
| _ -> raise Type_error


let rec typeof (en:Ast.env) (ast:Ast.exp) = match ast with
| EVar(x) -> lookup en x
| EVal(VFun(x,t,e)) -> TFun(t,typeof en e)
| EVal(VTrue) 
| EVal(VFalse) -> TBool
| EVal(VUnit) -> TUnit  
| EApp(x1,x2) -> (let t1 = typeof en x1 in
		  let t2 = typeof en x2 in
		 (match t1 with
		  | TFun(t11,t12) -> if t11 = t2 then t12 else raise Type_error
		  | _ -> raise Type_error))
| EIf(cond,b1,b2) ->( if (typeof en cond) = TBool then 
			let bb1 = typeof en b1 in 
			let bb2 = typeof en b2 in
			if bb1 = bb2 then bb1 else raise Type_error
			else raise Type_error)
| ESeq(e1,e2) -> (let t1 = typeof en e1 in
		  let t2 = typeof en e2 in
		 if t1 = TUnit then t2 else raise Type_error)
 
