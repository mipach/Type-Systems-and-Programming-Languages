(* Homework 1, CS490.40, UoC.  Instr: Polyvios Pratikakis.  Fall 2015. *)

(* Due 19 October, 23:59 *)

(* You should submit a homework1.ml file that implements this
 * interface file, and can be compiled with:
 * $ ocamlc -c homework1.mli homework1.ml
 *)


(************************************************************************
 * A: Lists and recursion
 * 
 * A.1: Write an OCaml function that takes a list and returns the last
 * element of the list.  You can assume the list will not be empty.
 *)
val last_element: 'a list -> 'a


(*
 * A.2: Write an Ocaml function that takes two lists of integers and
 * returns a list of the numbers that occur in the first, but not the
 * second.
 *)
val list_minus: int list -> int list -> int list


(*
 * A.3: Write an OCaml function that takes three arguments: another
 * function of one argument, a list, and an integer n.  You should
 * apply the input function on every n-th element of the input list
 * and return a list of the results.
 *)
val map_nth : ('a -> 'a) -> 'a list -> int -> 'a list


(************************************************************************
 * B: N-base arithmetic (where N >= 2)
 * Assume that a list of ints contains the digits of a number in N-based
 * representation.  For example, for binary arithmetic it would contain
 * 1s and 0s, with the head of the list being the least significant bit,
 * and the last element being the most significant bit, with 4 being
 * the list [0;0;1].
 *)

(*
 * Write OCaml functions to do the following, (without converting
 * n-base representation into OCaml int representation and back):
 * 
 * B.1: Given an integer n for the base, and an int list, check whether
 * the list is an n-base representation of a number.
 *)
val check_nbase : int -> int list -> bool

(*
 * B.2: Write a function of three arguments: an integer n for the
 * base, and two lists with an n-base represenation.  Subtract the
 * second from the first, and return a list with the n-base
 * representation of the difference.
 *)
val subtract_nbase: int -> int list -> int list -> int list

(*
 * B.3: Write an addition function add_n that takes three arguments:
 * an integer n for the base and two lists of integers representing
 * n-base numbers.  It should multiply the numbers and return an
 * integer list of the n-base representation of the product.
 *)
val multiply_nbase: int -> int list -> int list -> int list


(************************************************************************
 * C: Boolean syntax trees
 * Assume an abstract syntax tree of simple boolean formulas given by
 * the following type.  Remember you have to redefine the type in the
 * implementation file.
 *)

type bool_ast =
    True
  | False
  | And of bool_ast * bool_ast
  | Or of bool_ast * bool_ast
  | Not of bool_ast

(*
 * C.1: Write an OCaml function that takes a syntax tree and evaluates
 * it to true or false.
 *)
val eval_ast : bool_ast -> bool

(*
 * C.2: Write an OCaml function that takes a syntax tree and checks if
 * it contains any identities (an identity on A is the term "A and
 * A").
 *)
val has_identity: bool_ast -> bool

(*
 * C.3: Write an OCaml function that takes a syntax tree and
 * rewrites all negations of products (Not (A and B)) to sums of
 * negations ((Not A) or (Not B)), returning the resulting tree.
 *)
val rewrite_demorgan: bool_ast -> bool_ast

