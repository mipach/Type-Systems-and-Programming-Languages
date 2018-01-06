(********************************************************************
 * CS546 - http://www.csd.uoc.gr/~hy546/
 *
 * This file is part of homework 2
 *)

{
  open Parser
}
rule token = parse
    [' ' '\t' '\n']     { token lexbuf }
  | ['0'-'9']+ as lxm   { INT(int_of_string lxm) }
  | "fun"               { LAMBDA }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '.'                 { DOT }
  | ['a'-'z']+ as id    { IDENT (id) }
  | eof                 { EOF }
