
type id = string
type bop =
  |Add
  |Mult
  |Subt
  |Div
  |Exp
  |Mod
  |Log
  |Equals
  |Func of string

type unop = 
  |Sin 
  |Cos 
  |Tan 
  |Sec 
  |Csc 
  |Cot
  |Func_u of string

module Env = Map.Make (String)

type env = expr Env.t
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