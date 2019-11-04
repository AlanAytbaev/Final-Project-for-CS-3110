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
  | Float _ | Bool _ -> true
  | Var _ |Let _ |Binop _ | If _ -> false

(** [step e] takes a single step of evaluation of [e]. *)
let rec step (curr_env:Environment.t) : expr -> expr = function
  | Float _ -> failwith "Does not step"
  | Var _ -> failwith "can't do"
  | Bool _ -> failwith "naw"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
    step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 ->
    Binop (bop, e1, step curr_env e2)
  | Binop (bop, e1, e2) -> Binop (bop, step curr_env e1, e2)
  | Let (x, Float e1) -> let y = (Environment.add_binding x e1 curr_env) in Float(Environment.get_val x y)
  | Let (x, e1) -> Let (x, step curr_env e1)
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Float _, _, _) -> failwith "if_guard_err"
  | If (e1, e2, e3) -> If (step curr_env e1, e2, e3)


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

let rec main () =
  let curr_env = Environment.empty in
  ANSITerminal.print_string [red] ">";
  match String.trim (String.lowercase_ascii (read_line())) with
  |"quit" -> ()
  |e -> print_endline (interp e curr_env); print_endline ""; main ()


let () = main ()

let test_function x = x
