open Ast

module Graphing_Functions = struct
  let plot_func (f : Ast.expr list -> Ast.expr) =
    Float 0.0
end

module Graphing_CFU = struct

  let operation_list = [
    ("graph", Graphing_Functions.plot_func)
  ]
  let find_function (identifier : string) =
    match List.assoc_opt identifier operation_list with
    |Some f -> f
    |None -> failwith (identifier^" is not a valid imported function")
end
