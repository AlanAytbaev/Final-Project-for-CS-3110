module type Matrix_Funcs = sig
  val make_matrix : float -> float -> float array array
end1

module Matrix_Functions : Matrix_Funcs = struct
  let make_matrix (x : float) (y : float) =
    Array.make_matrix (int_of_float x) (int_of_float y) 0.0
end
