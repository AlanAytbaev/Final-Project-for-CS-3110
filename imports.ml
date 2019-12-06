open Arithmetic
open Trigonometric
open Statistics
open Ast

module type Imports_Sig = sig
  (** [find s] is the operation that is associated with [s] in the operation
      list *)
  val find_function : string -> (value list -> value)
end

module Imports = struct
  let stats = Statistics_CFU.operation_list
  let arith = Arithmetic_CFU.operation_list
  let trig = Trigonometric_CFU.operation_list

  let rec combine_lists lists acc =
    match lists with
    | h::t -> combine_lists t (List.append acc h)
    | [] -> acc

  let cfu_list = [arith;trig;]

  let operation_list = combine_lists cfu_list []

  let find_function (identifier : string) =
    match List.assoc_opt identifier operation_list with
    |Some f -> f
    |None -> failwith (identifier^" is not a valid imported function")
end