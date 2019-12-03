
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

and stats_type = float list -> float
and graph_type = value * env -> expr

and fun_ext = 
  | ExtFun of stats_type
  | GExtFun of graph_type

and result = 
  |Result of value
and
  id = string
and
  bop =
  |Add
  |Mult
  |Subt
  |Div
  |Exp
  |Mod
  |Log
  |Equals
  |Func of string
and 
  unop = 
  |Sin 
  |Cos 
  |Tan 
  |Sec 
  |Csc 
  |Cot
  |Func_u of string

and expr =
  |Boolean of bool
  |Var of id
  |String of string
  |Float of float
  |Binop of bop * expr * expr
  |Unop of unop * expr
  |If of expr * expr * expr
  |Let of id * expr * expr
  |Fun of string list * expr
  |FunApp of expr * expr list


and defn = 
  |DLet of string * expr

and phrase = 
  | Expr of expr
  | Defn of defn
