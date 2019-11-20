open Ast
open Stdlib
open ANSITerminal
open Arithmetic
open Environment
open Lexing
open Printf
open Trigonometric
open Statistics 

module Env = Map.Make(String)
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
and fun_ext = ExtFun of stats_type

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

let return_head = function
  |h::t -> h 
  | _ -> failwith "This case is not reached"

let rec helper_expr_to_float = function 
  |[] -> []
  |VFloat f:: t -> f:: helper_expr_to_float t 
  | _ -> failwith "not right type of argument"

(** [string_of_expr e] converts [e] to a string.
    Requires: [e] is an expression. *)
let string_of_expr e =
  match e with
  |Float i -> string_of_float i
  |Boolean b -> string_of_bool b
  |String s -> s
  |If _ -> "if expression"
  |Let _ -> "let"
  |Fun (s, e)-> "fun"
  |FunApp (s, e) -> "fun app"
  |Var x -> x
  |Binop _ -> "binop"
  |Unop _ -> "unop"

let string_of_value  = function 
  |VBool b -> string_of_bool b
  |VFloat s -> string_of_float s
  |VString s -> "\"" ^ String.escaped s ^ "\""
  |VBinop b -> "\"" ^ String.escaped b ^ "\""
  |VId x -> "\"" ^ String.escaped x ^ "\""
  |Closure (x, e, env) -> "<closure>"
  |Extern e -> "<extern>"


let load_file f env =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (Bytes.unsafe_to_string s)


let rec step (curr_env:env) expr = 
  match expr with 
  | Float x -> VFloat ( x)
  | Var y ->  Env.find y curr_env
  | String s ->  (VString s)
  | Boolean b -> (VBool b)
  | Binop (bop, e1, e2)  ->
    step_bop bop e1 e2 curr_env
  | Let (x, e1, e2) -> eval_let_expr curr_env x e1 e2
  | If (e1, e2, e3) -> eval_if e1 e2 e3 curr_env
  | Fun (s,e) -> Closure (s, e, curr_env)
  | FunApp (e1,e2) -> eval_fun e1 e2 curr_env
  | Unop (unop, e) -> eval_unop unop e curr_env


and eval_unop uop e env = 
  let v = step env e in 
  match uop, v with 
  |Func_u str, VFloat a -> VFloat (((Trigonometric_CFU.find_function str) [a]))
  |_ -> failwith "precondition violated"

and eval_if e1 e2 e3 env = 
  let v = step env e1 in 
  match v with 
  |VBool true ->  step env e2 
  |VBool false -> step env e3
  |VFloat f -> if f = 1. then step env e2 else step env e3 
  | _ -> failwith "if guard error"



(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2].  Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 env = 
  let e1' = step env e1 in 
  let e2' = step env e2 in 
  match bop, e1', e2' with
  | Func str, VFloat a, VFloat b ->
    (VFloat (((Arithmetic_CFU.find_function str) [a;b])))
  | _ -> failwith "precondition violated - step bop"

and string_of_var e = match e with 
  | Var e -> e
  |_ -> failwith "not vaild identifier for let statement" 

and eval_let_expr env x e1 e2 =
  let v1 = step env e1  in 
  let env' = Env.add x v1 env in
  let v = step env' e2 in v


and eval_fun e1 e2 env = 
  let v' = step env e1  in 
  match v' with 
  |Closure( s, e, env') -> begin 
      let v2 = eval_id_list e2 env in 
      if (List.length s <> List.length v2) then failwith "wrong number of arguments" 
      else
        let base_env = env' in 
        let env_for_body = add_bindings s v2 base_env in
        step env_for_body e
    end
  |Extern (ExtFun f) ->  let v2 = eval_id_list e2 env  in 
    VFloat (f (helper_expr_to_float v2))
  |_-> failwith "function failure"

and eval_id_list e2 env = 
  match e2 with
  |[] -> []
  |e :: t -> (step env e ):: (eval_id_list t env)

and add_bindings ids values env = 
  match (ids, values) with 
  | ([], []) -> env
  | (id :: idt, v :: vt) -> let env' =  Env.add id (v) env in 
    add_bindings idt vt env'
  | _ -> env


let eval_let_defn (env1:env) id e = 
  let v = step env1 e  in
  let env' = Env.add id v env1 in
  (v, env')

let eval_defn env e =
  match e with
  |DLet (id, e1) -> eval_let_defn env id e1

let rec eval_phrase env exp =
  match exp with
  |Expr e -> (step env e, env)
  |Defn d -> eval_defn env d


(** [interp s] interprets [s] by parsing and evaluating it. *)
let interp (s : string) (curr_env: env) : (string * env) =
  try (
    let expr = s |> parse_phrase |> eval_phrase curr_env in 
    let v' = fst expr in 
    let env = snd expr in 
    let str = string_of_value v' in
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

let initial_env =  Env.empty 
                   |> Env.add "mean" (Extern (ExtFun (Statistics_CFU.find_function "mean")))
                   |> Env.add "median" (Extern (ExtFun (Statistics_CFU.find_function "median"))) 
                   |> Env.add "stdev" (Extern (ExtFun (Statistics_CFU.find_function "stdev"))) 
                   |> Env.add "min" (Extern (ExtFun (Statistics_CFU.find_function "min"))) 
                   |> Env.add "max" (Extern (ExtFun (Statistics_CFU.find_function "max")))
                   |> Env.add "range" (Extern (ExtFun (Statistics_CFU.find_function "range")))
                   |> Env.add "perm" (Extern (ExtFun (Statistics_CFU.find_function "perm")))
                   |> Env.add "comb" (Extern (ExtFun (Statistics_CFU.find_function "comb")))

let () = main () (initial_env)
