(* The type of the abstract syntax tree (AST). *)
type value = 
  | VInt of int
  | VBool of bool

type expr =
  | Var of string
  | Int of int 
  | Add of expr*expr 
  | Mult of expr*expr
  | Let of string*expr*expr 
  | If of expr*expr*expr
  | Bool of bool 
  | Le of expr*expr

