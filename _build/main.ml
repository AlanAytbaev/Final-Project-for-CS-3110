open Ast
open Stdlib
open ANSITerminal
open Arithmetic

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Float _ -> true
  | Binop _ -> false

(** [step e] takes a single step of evaluation of [e]. *)
let rec step : expr -> expr = function
  | Float _ -> failwith "Does not step"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
    step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 ->
    Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2].  Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 = match bop, e1, e2 with
  | Add, Float a, Float b -> Float(Arithmetic_Functions.add2 [a;b]) (* this is where i would call the methods from arithmetic.ml *)
  | Mult, Float a, Float b -> Float (Arithmetic_Functions.multiply [a;b])
  | Subt, Float a, Float b -> Float (Arithmetic_Functions.subtract [a;b])
  | Div, Float a, Float b -> Float (Arithmetic_Functions.divide [a;b])
  | _ -> failwith "precondition violated"

(** [eval e] fully evaluates [e] to a value. *)
let rec eval (e : expr) : expr =
  if is_value e then e
  else e |> step |> eval

(** [string_of_val e] converts [e] to a string.
    Requires: [e] is a value. *)
let string_of_val (e : expr) : string =
  match e with
  | Float i -> string_of_float i
  | Binop _ -> failwith "precondition violated"

(** [interp s] interprets [s] by parsing and evaluating it. *)
let interp (s : string) : string =
  s |> parse |> eval |> string_of_val

let rec main () =
  ANSITerminal.print_string [red] ">";
  match String.trim (String.lowercase_ascii (read_line())) with
  |"quit" -> ()
  |e -> print_endline (interp e); print_endline ""; main ()

let () = main ()

