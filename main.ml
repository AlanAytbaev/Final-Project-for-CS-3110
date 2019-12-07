open Ast
open Stdlib
open ANSITerminal
open Arithmetic
open Environment
open Lexing
open Printf
open Trigonometric
open Statistics
open Imports

module type Main_Sig = sig
  val interp : string -> env -> (string * env)
  val run : unit -> unit
end

module Main = struct

  (**Exceptions *)
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

  (** [helper_expr_to_float lst] is the float list representation
      of [lst] is all values of [lst] are floats *)
  let rec values_to_floats = function 
    |[] -> []
    |VFloat f :: t -> f :: values_to_floats t 
    | _ -> failwith "not right type of argument"

  (** TODO: DOCUMENT *)
  let rec string_of_floatlst lst = 
    match lst with 
    |[] -> ""
    |h :: t -> if t = [] then 
        string_of_float h ^"" ^ string_of_floatlst t else 
        string_of_float h ^", " ^ string_of_floatlst t

  (** TODO: DOCUMENT *)
  let rec matrix_to_string_helper lsts fst_element= 
    match lsts with 
    |[] -> ""
    |h::t -> if (h == fst_element) then
        "|"^ string_of_floatlst h ^"|" ^ matrix_to_string_helper t fst_element else 
        "\n |"^ string_of_floatlst h ^"|" ^ matrix_to_string_helper t fst_element

  (** TODO: DOCUMENT *)
  let matrix_to_string m = 
    let lst = Array.to_list (Array.map Array.to_list m) in 
    let s = "["^ (matrix_to_string_helper lst (List.nth lst 0))^ "]"in s

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
    |VRow r -> 
      let s = Array.fold_left (fun acc s -> acc^ " " ^ (string_of_float s)) "" r 
      in "["^s^"]"
    |VFloatList _ -> "float list values"
    |VMatrix m -> matrix_to_string m

  let load_file f env =
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    (Bytes.unsafe_to_string s)

  let unwrap_float v =
    match v with
    | VFloat x -> x
    | _ -> failwith "This cannot occur - arithmetic.ml"

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
    |Func_u str, v1 -> (Imports.find_function str) [v1]
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
    | Func str, v1, v2 ->
      (Imports.find_function str) [v1;v2]
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
        if (List.length s <> List.length v2)
        then 
          failwith "wrong number of arguments" 
        else
          let base_env = env' in 
          let env_for_body = add_bindings s v2 base_env in
          step env_for_body e
      end
    |Extern (ExtFun f) ->  
      let v2 = eval_id_list e2 env  in 
      (f v2)
    |Extern (GExtFun g) -> 
      let v2 = (eval_id_list e2 env) in
      step env (g (List.nth v2 0 , List.nth v2 1, List.nth v2 2, env))
    |_-> failwith "function failure"

  and eval_id_list e2 env = 
    match e2 with
    |[] -> []
    |e :: t -> (step env e ):: (eval_id_list t env)

  (** TODO: DOCUMENT *)
  and eval_id_list_floats e2 env = 
    match e2 with
    |[] -> []
    |e :: t -> (step env (e )):: (eval_id_list_floats (t) env)

  and add_bindings ids values env = 
    match (ids, values) with 
    | ([], []) -> env
    | (id :: idt, v :: vt) -> let env' =  Env.add id (v) env in 
      add_bindings idt vt env'
    | _ -> env

  (** TODO: DOCUMENT *)
  let rec eval_row_id_list lst env = 
    match lst with 
    |[] -> []
    |h :: t -> let h = Var h in (step env h) :: (eval_row_id_list t env)

  (** TODO: DOCUMENT *)
  let array_getter e = 
    match e with 
    |VRow e -> e
    |_ -> failwith "wrong type of arguement"

  (** TODO: DOCUMENT *)
  let rec array_updater lst array acc = 
    match lst with 
    |[] -> array
    |h :: t -> let ar = array_getter h in 
      let () = array.(acc) <- ar in array_updater t array (acc+1)

  (** TODO: DOCUMENT *)
  let eval_mlet_defn id e env = 
    let lst_of_arrays = eval_row_id_list e env in 
    let column_length = List.nth lst_of_arrays 0 
                        |> array_getter 
                        |> Array.length in 
    let array = Array.make_matrix  (List.length e) column_length 0. in 
    let array' = VMatrix (array_updater lst_of_arrays array 0) in 
    let env' = Env.add id array' env in 
    (array', env')

  (** TODO: DOCUMENT *)
  let eval_mrow x lst curr_env = 
    let a = eval_id_list lst curr_env in
    let a' = values_to_floats a in  
    let array = Array.make (List.length a') 0. in 
    let () = for i = 0 to ((List.length a') - 1) do
        array.(i) <- List.nth a' i
      done in
    let env = Env.add x (VRow array) curr_env in 
    (VRow array, env)

  (** TODO: DOCUMENT *)
  let eval_mlet_defn id e env = 
    let lst_of_arrays = eval_row_id_list e env in 
    let column_length = List.nth lst_of_arrays 0 
                        |> array_getter 
                        |> Array.length in 
    let array = Array.make_matrix  (List.length e) column_length 0. in 
    let array' = VMatrix (array_updater lst_of_arrays array 0) in 
    let env' = Env.add id array' env in 
    (array', env')

  let eval_let_defn (env1:env) id e = 
    let v = step env1 e  in
    let env' = Env.add id v env1 in
    (v, env')

  (** TODO: DOCUMENT *)
  let float_getter v = 
    match v with 
    |VFloat v -> v 
    |_ -> failwith "invalid argument"

  (** TODO: DOCUMENT  - duplicate function??? *)
  let rec eval_id_lst_flts lst env = 
    match lst with
    |[] -> []
    |e :: t -> ( float_getter (step env e)) :: (eval_id_lst_flts t env)

  (** TODO: DOCUMENT *)
  let eval_slet id e env = 
    let vlst = (VFloatList (eval_id_lst_flts e env)) in 
    let env' = Env.add id vlst env in 
    (vlst, env')

  (** [eval_defn env e] is the (v, env') where [v] is such that
      [<env, e> ==> <v, env'>]. *)
  let eval_defn env e =
    match e with
    |MRow (id, e) -> eval_mrow id e env
    |DLet (id, e1) -> eval_let_defn env id e1
    |MLet (id, e) -> eval_mlet_defn id e env
    |SLet (id, e) -> eval_slet id e env 

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

  (**BEGIN Externs *)
  let graph (c, left_bound, right_bound, env) = 
    let (ids, f, env') = 
      match c with
      | Closure (ids, f , env') -> (ids, f, env')
      | _ -> failwith "Cannot graph a non-closure main.ml : graph"
    in
    let out_file = "data.dat" in
    let out_handle = open_out out_file in 
    let left_int = (left_bound |> unwrap_float |> int_of_float)*100 in
    let right_int = (right_bound |> unwrap_float |> int_of_float)*100 in 
    for x=left_int to right_int do
      try 
        let y = 
          step 
            env 
            (FunApp (Fun (["x"], f), [Float (Float.of_int (x / 100))]))
        in
        fprintf out_handle "%d %f\n" (x/100) (y |> unwrap_float)
      with
      | e -> ()
    done;
    close_out out_handle;
    let _ = Sys.command "gnuplot -c gnuplot_script.txt" in 
    Boolean true

  let derivative_helper (c : value) (x1_val : value) (h_val : value) =
    let (ids, expr, env) = 
      match c with
      | Closure (ids, expr , env') -> (ids, expr, env')
      | _ -> failwith "Cannot graph a non-closure main.ml : graph"
    in
    let f = (Fun (["x"], expr)) in
    let x1 = x1_val |> unwrap_float in
    let h = h_val |> unwrap_float in 
    let y1 = step env (FunApp (f,[Float x1])) |> unwrap_float in
    let x2 = Float.add x1 h in 
    let y2 = step env (FunApp (f,[Float x2])) |> unwrap_float in
    (Float.div (Float.sub y2 y1) (h_val |> unwrap_float ))

  let derivative (v : value list) = 
    let c = List.nth v 0 in 
    let x1_val = List.nth v 1 in 
    let h_val = List.nth v 2 in
    VFloat (derivative_helper c x1_val h_val)

  let trapezoid (c : value) (v : float) (v1 : float) =
    let (ids, expr, env) = 
      match c with
      | Closure (ids, expr , env') -> (ids, expr, env')
      | _ -> failwith "Cannot graph a non-closure main.ml : graph"
    in
    let f = (Fun (["x"], expr)) in
    let left_height = step env (FunApp (f,[Float v])) |> unwrap_float in
    let right_height = step env (FunApp (f, [Float v1])) |> unwrap_float in 
    Float.mul
      (Float.mul 0.5 (Float.add (left_height) (right_height))) 
      (Float.sub v1 v)

  let rec integrate_helper 
      (f : value) (v : float) (v1 : float) (acc : float) =
    if v > (v1 -. 0.09) && v < (v1 +. 0.09) then
      acc 
    else if v < v1 then
      integrate_helper 
        f 
        (Float.add v 0.1) 
        v1 
        (Float.add acc (trapezoid f v (Float.add v 0.1)))
    else 
      integrate_helper 
        f 
        (Float.add v (- 0.1) )
        v1 
        (Float.add acc (trapezoid f v (Float.add v (- 0.1))))

  let integrate (v : value list) = 
    VFloat (integrate_helper 
              (List.nth v 0) 
              (List.nth v 1 |> unwrap_float) 
              (List.nth v 2 |> unwrap_float)
              (0.0))

  (**END Externs *)

  let initial_env =  

    Env.merge 
      (fun key a b -> 
         match (a, b) with 
         | (Some _, Some _) -> failwith "You have conflicting imports"
         | (Some _, None) -> a
         | (None, Some _) -> b
         | (None, None) -> failwith "This cannot happen"
      )
      Imports.functions_map
      (Env.empty 
       |> Env.add "graph" (Extern (GExtFun (graph)))
       |> Env.add "deriv" (Extern (ExtFun (derivative)))
       |> Env.add "integ" (Extern (ExtFun (integrate)))
       |> Env.add "pi" (VFloat 3.14))

  let run = fun () ->
    main () (initial_env)
end