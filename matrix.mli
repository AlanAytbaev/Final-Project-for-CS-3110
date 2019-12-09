open Ast

(** An abstract module type that is meant to structure the CFU modules 
    (ie. Aritmetic functions, calculus functions, statistics functions). 
    A module that matches [CFU_sig] is suitable for use in the [Calc] module. *)
module type CFU_sig = sig
  val operation_list : (string * ( value list -> value)) list
end

module type Matrix_Funcs = sig

  (** [make_matrix x y]  is a matrix with x dimension [x] and y dimension [y] *)
  val make_matrix : value list -> value


  (** [add_matrix m1 m2] is a matrix that is the result of [m1] + [m2] *)
  val add_matrix : value list -> value


  (** [sub_matrix m1 m2] is a matrix that is the result of [m1] - [m2] *)
  val sub_matrix : value list -> value

  val dot_product_matrix : value list -> value

  val echelon_form : value list -> value

  val reduced_echelon_form : value list -> value
end

module Matrix_Functions : Matrix_Funcs
module Matrix_CFU : CFU_sig