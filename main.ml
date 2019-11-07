open Ast
open Stdlib
open ANSITerminal
open Arithmetic
open Environment
open Lexing 


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


let parse_expr = parse Parser.prog


(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Float _ | Boolean _ -> true
  | Var _ |Let _ |Binop _ | If _ -> false


let rec step curr_env expr = 
  match expr with 
  | Float x -> Float x
  | Var y -> 
    Float (Environment.get_val y curr_env)
  | Boolean _ -> failwith "naw"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
    step_bop bop e1 e2  
  | Binop (bop, e1, e2) -> 
    Binop (bop, step curr_env e1, step curr_env e2)
  | Let (x, Float e1, e2) -> let y = (Environment.add_binding x e1 curr_env) in step y e2
  | Let (x, e1, e2) -> Let (x, step curr_env e1, e2)
  | If (Float 1.0, e2, _) -> e2
  | If (Float 0.0, _, e3) -> e3
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
  |Boolean b -> string_of_bool b
  |_ -> failwith "precondition violated"

(** [interp s] interprets [s] by parsing and evaluating it. *)
let interp (s : string) (curr_env: Environment.t) : string =
  try (
    s |> parse_expr |> eval curr_env |> string_of_val )
  with
  |SyntaxError s |Failure s -> s


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
    |s -> print_endline s;
      print_endline ""; 
      main () curr_env

let () = main () (Environment.empty)

