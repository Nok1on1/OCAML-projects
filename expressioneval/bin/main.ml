type rat = int * int;;
type var = string;;
type unary_op = Neg;;
type binary_op = Add | Sub | Mul | Div;;
type expr = Const of rat
      | UnOp of unary_op * expr
      | BinOp of binary_op * expr * expr
      | Var of var
      | Func of var * expr
      | Bind of var * expr * expr
      | App of expr * expr
      | Ite of expr * expr * expr;;

type value = Rat of rat | Fun of var * state * expr and state = var -> value option;;

let rec eval_expr (state : state) (expr : expr) :value = match expr with
| pattern -> pattern

;;