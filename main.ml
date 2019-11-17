open Ast
open Stdlib
open ANSITerminal
open Arithmetic
open Environment
open Lexing 

module Env = Map.Make(String)
type env = value Env.t

and value = 
  |Val of Ast.expr
  |Closure of string list * expr * env



exception SyntaxError of string
exception UnexpectedError of string

(** [parse_error lexbuf] is the error to raise when parser/lexeing raises 
    a parsing or lexing error *)
let parse_error lexbuf = raise (SyntaxError "Syntax error, please try again")

(** [unexp_error lexbuf] is the error to raise when parser/lexeing fails *)
let unexp_err lexbuf = raise (UnexpectedError "Unexepcted error, please try again")


let parse parser_start s =
  let lexbuf = from_string s in
  try parser_start Lexer.read lexbuf with
  | Parser.Error | Lexer.Syntax_error -> parse_error lexbuf
  | Failure s -> unexp_err s 


let parse_phrase = parse Parser.prog


(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Float _ | Boolean _ -> true
  | Var _ |Let _ |Binop _ | If _ | Fun _ | FunApp _-> false


let rec step (curr_env:env) expr = 
  match expr with 
  | Float x -> Val (Float x)
  | Var y -> Env.find y curr_env
  | Boolean _ -> failwith "naw"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
    Val (step_bop bop e1 e2 )
  | Binop (bop, e1, e2) -> 
    Val (Binop (bop, get_expr(step curr_env e1), get_expr(step curr_env e2)))
  | Let (x, e1, e2) -> Val (eval_let_expr curr_env x e1 e2)
  | If (Float 1.0, e2, _) -> Val (e2)
  | If (Float 0.0, _, e3) -> Val (e3)
  | If (Float _, _, _) -> failwith "if_guard_err"
  | If (e1, e2, e3) -> Val (If (get_expr(step curr_env e1), e2, e3))
  | Fun (s,e) -> Closure (s,e,curr_env)
  | FunApp (e1,e2) -> eval_fun e1 e2 curr_env

and get_expr value = match value with
  |Val x -> x
  |Closure (id,e,env)-> e

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2].  Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 = match bop, e1, e2 with
  | Func str, Float a, Float b ->
    Float ((Arithmetic_CFU.find_function str) [a;b])
  | _ -> failwith "precondition violated"


and eval_let_expr env x e1 e2 =
  let v1 = step env e1 in
  let env' = Env.add x v1 env in
  let v = step env' e2 in get_expr v

and eval_fun_app env envcl id_list e_list e2 = 
  match id_list, e_list with 
  |[],[] -> step envcl e2
  |h1::t1, h2::t2 -> begin 
      match step env h1 with
      |Val v -> let new_env = Env.add h2 (Val v) envcl in 
        eval_fun_app env new_env t1 t2 e2 
      |_ -> failwith "No Closure"
    end 
  |_,_ -> failwith "Wrong Argument" 

and eval_fun e e_list env = match step env e with 
  |Val x -> failwith "Not a function"
  |Closure (id_list, e1, envcl) -> eval_fun_app env envcl e_list id_list  e1 

(** [eval e] fully evaluates [e] to a value. *)
let rec eval (curr_env:env) (e : expr) =
  if is_value e then (e, curr_env)
  else e |> step curr_env |> get_expr |> eval curr_env

(** [string_of_val e] converts [e] to a string.
    Requires: [e] is a value. *)
let string_of_val e  =
  match (fst e) with
  |Float i -> string_of_float i
  |Boolean b -> string_of_bool b
  |_ -> failwith "precondition violated"

let eval_let_defn (env1:env) id e = 
  let v = step env1 e in
  let env' = Env.add id v env1 in
  (get_expr v, env')

let eval_defn env e = 
  match e with 
  |DLet (id, e1) -> eval_let_defn env id e1

let rec eval_phrase env exp =
  match exp with 
  |Expr e -> eval env e
  |Defn d -> eval_defn env d

(** [interp s] interprets [s] by parsing and evaluating it. *)
(* let interp_expr (s : string) (curr_env: env) : string =
   try (
    s |> parse_phrase |> eval_phrase curr_env |> string_of_val )
   with
   |SyntaxError s |Failure s -> s *)

(** [interp s] interprets [s] by parsing and evaluating it. *)
let interp_defn (s : string) (curr_env: env) : (string * env) =
  try (
    let (v, env) = s |> parse_phrase |> eval_phrase curr_env in 
    let s = (v, env) |> string_of_val in 
    (s, env)
  )
  with
  |SyntaxError s |Failure s -> (s, curr_env)



let rec help_command_helper chnl = 
  match input_line chnl with
  |s -> print_endline s; help_command_helper chnl
  |exception End_of_file -> close_in chnl

let rec main () curr_env =
  ANSITerminal.print_string [red] ">";
  match String.trim (String.lowercase_ascii (read_line())) with
  |"quit" -> ()
  |"help" -> let chnl = open_in "help.txt" in help_command_helper chnl; main () curr_env
  |e -> match (interp_defn e curr_env) with
    |exception Not_found -> print_endline "Not a valid command please try again"; main () curr_env
    |(s, env) -> print_endline s;
      print_endline ""; 
      main () env

let () = main () (Env.empty)

