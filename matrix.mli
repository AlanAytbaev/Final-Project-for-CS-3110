open Ast

(** An abstract module type that is meant to structure the CFU modules 
    (ie. Aritmetic functions, calculus functions, statistics functions). 
    A module that matches [CFU_sig] is suitable for use in the [Imports] 
    module. *)
module type CFU_sig = sig
  (** An [operation_list] is an association list that maps operation 
      symbols to functions *)
  val operation_list : (string * ( value list -> value)) list
end

module type Matrix_Funcs = sig

  (** [make_matrix v]  is a matrix with x dimension as the first element of [v]
      and y dimension as the second element of [v] 
      Requires: [v] is of length = 2 and contains float values *)
  val make_matrix : value list -> value

  (** [add_matrix v] is a matrix that is the result of adding the first element
      of [v] and the second element of [v] 
      Requires: [v] is of length = 2 and contains elements of matrix type *)
  val add_matrix : value list -> value

  (** [sub_matrix v] is a matrix that is the result of subtracting the second 
      element of [v] from the first element of [v] 
      Requires: [v] is of length = 2 and contains elements of matrix type *)
  val sub_matrix : value list -> value

  (**[dot_product_matrix v] is a matrix that is the scalar product of the first 
     and second elements of [v] 
     Requires: [v] is of length = 2, contains elements of matrix type and 
     matrices of dimensions (mxn) and (nxr) where m,n,r are positive integers *)
  val dot_product_matrix : value list -> value

  (** [echelon_form v] is a matrix that is equivalent to the echelon form of the
      first element of [v]
      Requires: [v] is of length = 1 and contains element of matrix type *)
  val echelon_form : value list -> value

  (** [reduced_echelon_form v] is a matrix that is equivalent to the reduced 
      echelon form of the first element of [v]
        Requires: [v] is of length = 1 and contains element of matrix type *)
  val reduced_echelon_form : value list -> value

  (** [determinant v] is a matrix that is equivalent to determinant of the 
      matrix that is the first element of [v]
        Requires: [v] is of length = 1 and contains element of matrix type *)
  val determinant : value list -> value

end

(** [Matrix_Functions] is a module that implements the function values defined 
    in [Matrix_Funcs]. Contains all the matrix functionalities in CAMLCALC *)
module Matrix_Functions : Matrix_Funcs

(** [Matrix_CFU] is a module of type [CFU_sig] which contains the 
    [operation_list] with matrix functions *)
module Matrix_CFU : CFU_sig