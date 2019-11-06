open Ast
open Stdlib
open ANSITerminal
open Arithmetic
open Environment


(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr = 
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast



(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Float _ -> true
  | Var _ |Let _ |Binop _  -> false


let rec step curr_env expr = 
  match expr with 
  | Float x -> Float x
  | Var y -> 
    Float (Environment.get_val y curr_env)
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
    step_bop bop e1 e2  
  | Binop (bop, e1, e2) -> 
    Binop (bop, step curr_env e1, step curr_env e2)
  | Let (x, Float e1, e2) -> let y = (Environment.add_binding x e1 curr_env) in step y e2
  | Let (x, e1, e2) -> Let (x, step curr_env e1, e2)

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2].  Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 = match bop, e1, e2 with
  | Func str, Float a, Float b -> 
    Float ((Arithmetic_CFU.find_function str) [a;b])
  | _ -> failwith "precondition violated"


(** [eval e] fully evaluates [e] to a value. *)
let rec eval (curr_env:Environment.t) (e : expr) : expr =
  if is_value e then e
  else e |> step curr_env |> eval curr_env

(** [string_of_val e] converts [e] to a string.
    Requires: [e] is a value. *)
let string_of_val (e : expr) : string =
  match e with
  |Float i -> string_of_float i
  |_ -> failwith "precondition violated"

(** [interp s] interprets [s] by parsing and evaluating it. *)
let interp (s : string) (curr_env: Environment.t) : string =
  s |> parse |> eval curr_env |> string_of_val


let rec main () curr_env =
  ANSITerminal.print_string [red] ">";
  match String.trim (String.lowercase_ascii (read_line())) with
  |"quit" -> ()
  |e -> match (interp e curr_env) with
    |exception Not_found -> main () curr_env
    |s -> print_endline s;
      print_endline ""; 
      main () curr_env

let () = main () (Environment.empty)


