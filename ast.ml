
module Env = Map.Make (String)

type env = value Env.t

and value =
  |VBool of bool
  |VFloat of float
  |VString of string
  |VBinop of string
  |VId of id
  |Closure of id list * expr * env 
  |Extern of fun_ext
  |VMatrix of float array array
  |VFloatList of float list
  |VRow of float array

and matrix_type = value list -> env -> value

and set_type = float array -> float array  -> float array

and graph_type = value * value * value * env -> expr

and ext_type = value list -> value

and fun_ext = 
  | ExtFun of ext_type
  | GExtFun of graph_type
  | MExtFun of matrix_type

and result = 
  |Result of value

and
  id = string

and
  bop =
  |Func of string
and 

  unop = 
  |Func_u of string

and expr =
  |Boolean of bool
  |Var of id
  |String of string
  |Float of float
  |Binop of bop * expr * expr
  |Sequence of expr * expr
  |DSequence of defn * expr
  |Unop of unop * expr
  |If of expr * expr * expr
  |Let of id * expr * expr
  |Fun of string list * expr
  |FunApp of expr * expr list
  |Arr of expr list

and defn = 
  |DLet of string * expr
  |MRow of string * expr list
  |MLet of string * id list
  |SLet of string * expr list

and phrase = 
  | Expr of expr
  | Defn of defn