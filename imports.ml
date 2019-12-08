open Arithmetic
open Trigonometric
open Statistics
open Ast
open Matrix
open Myset
open Fibonacci
open Newset

module type Imports_Sig = sig
  (** [find s] is the operation that is associated with [s] in the operation
      list *)
  val find_function : string -> (value list -> value)
  val functions_map : env
end

module Imports = struct
  let stats = Statistics_CFU.operation_list
  let arith = Arithmetic_CFU.operation_list
  let trig = Trigonometric_CFU.operation_list
  let matrix = Matrix_CFU.operation_list
  let set = MySet_CFU.operation_list
  let fib = Fib_CFU.operation_list
  let newset = NewSet_CFU.operation_list

  let cfu_list = [arith;trig;matrix;set;fib;newset]

  let operation_list = List.append cfu_list [] |> List.flatten

  let rec map_of_functions operations acc = 
    match operations with
    | [] -> acc
    | h::t ->
      let name = fst h in 
      let f = snd h in
      let env' = Env.add name (Extern (ExtFun f)) acc in
      map_of_functions t env'

  let functions_map = map_of_functions operation_list Env.empty

  let find_function (identifier : string) =
    match List.assoc_opt identifier operation_list with
    |Some f -> f
    |None -> failwith (identifier^" is not a valid imported function")
end