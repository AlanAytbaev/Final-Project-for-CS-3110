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
  |Closure of id list * expr * env

and result = 
  |Result of value



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


(** [string_of_expr e] converts [e] to a string.
    Requires: [e] is an expression. *)
let string_of_expr e =
  match e with
  |Float i -> string_of_float i
  |Boolean b -> string_of_bool b
  |If _ -> "if expression"
  |Let _ -> "let"
  |Fun (s, e)-> "fun"
  |FunApp (s, e) -> "fun app"
  |Var x -> "var"
  |Binop _ -> "binop"

(** [string_of_val v] converts [v] to a string.
    Requires: [v] is a value. *)
let string_of_val v = 
  match v with 
  |Val e -> string_of_expr e
  |Closure _ -> "<Closure>"


(** [is_value e] is whether [e] is a value. *)
(* let is_value : expr -> bool = function
   | Float _ | Boolean _ -> true
   | Var _ |Let _ |Binop _ | If _ | Fun _ | FunApp _-> false *)

let is_prim expr = 
  match expr with 
  |Float x -> true
  |Boolean b -> true
  |_ -> false


let rec step (curr_env:env) expr = 
  match expr with 
  | Float x -> Val (Float x)
  | Var y ->  Env.find y curr_env
  | Boolean b -> Val (Boolean b)
  | Binop (bop, e1, e2) when is_prim e1 && is_prim e2 ->
    step_bop bop e1 e2
  | Binop (bop, e1, e2) -> 
    Val (Binop (bop, get_expr(step curr_env e1), get_expr(step curr_env e2)))
  | Let (x, e1, e2) -> eval_let_expr curr_env x e1 e2
  | If (Float 1.0, e2, _) -> Val (e2)
  | If (Float 0.0, _, e3) -> Val (e3)
  | If (Float _, _, _) -> failwith "if_guard_err"
  | If (e1, e2, e3) -> Val (If (get_expr(step curr_env e1), e2, e3))
  | Fun (s,e) -> Closure (s,e,curr_env)
  | FunApp (e1,e2) -> eval_fun e1 e2 curr_env
(* | Closure (e, env) -> Val (Closure (e, env)) *)

and get_expr value = match value with
  |Val x -> x
  |Closure (x, e, env) -> e


(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2].  Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 = match bop, e1, e2 with
  | Func str, Float a, Float b ->
    (Val (Float ((Arithmetic_CFU.find_function str) [a;b])))
  | _ -> failwith "precondition violated - step bop"

and string_of_var e = match e with 
  | Var e -> e
  |_ -> failwith "not vaild identifier for let statement" 

and eval_let_expr env x e1 e2 =
  let v1 = step env e1  in
  let x' = x in 
  let env' = Env.add x' v1 env in
  let v = step env' e2 in v


and eval_fun e1 e2 env = 
  print_endline "eval_fun";
  let v' = step env e1  in 
  match v' with 
  |Closure( s, e, env') -> begin 
      let v2 = eval_id_list e2 env' in 
      if (List.length s <> List.length v2) then failwith "wrong number of arguments" 
      else
        let env_for_body = add_bindings s v2 env' in
        step env_for_body e
    end
  |_-> failwith "function failure"

and eval_id_list e2 env = 
  match e2 with
  |[] -> []
  |e :: t -> if is_prim e then 
      (Val e) :: (eval_id_list t env)
    else
      (step env e ):: (eval_id_list t env)

and add_bindings ids values env = 
  match (ids, values) with 
  | ([], []) -> env
  | (id :: idt, v :: vt) -> let env' =  Env.add id (v) env in 
    add_bindings idt vt env'
  | _ -> env

(** [eval e] fully evaluates [e] to a value. *)
let rec eval (curr_env:env) (e : expr) =
  if is_prim e then (e, curr_env)
  else e |> step curr_env |> get_expr |> eval curr_env



let eval_let_defn (env1:env) id e = 
  if is_prim e then
    let v = Val e in 
    let env' = Env.add id v env1 in
    (e, env')
  else
    let v = step env1 e  in
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
let interp (s : string) (curr_env: env) : (string * env) =
  try (
    let expr = s |> parse_phrase |> eval_phrase curr_env in 
    let expr' = fst expr in 
    let env = snd expr in 
    let str = string_of_expr expr' in
    (str, env)
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
  |e -> match (interp e curr_env) with
    |exception Not_found -> print_endline "Not a valid command please try again"; main () curr_env
    |(s, env) -> print_endline s;
      print_endline ""; 
      main () env

let () = main () (Env.empty)

