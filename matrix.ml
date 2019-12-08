open Ast

module type CFU_sig = sig

  val operation_list : (string * ( value list -> value)) list

end

module type Matrix_Funcs = sig
  (* type matrix *)
  val make_matrix : value list -> value

  val add_matrix : value list -> value

  val sub_matrix : value list -> value

  val dot_product_matrix : value list -> value

  val echelon_form : value list -> value 

end

module Matrix_Functions : Matrix_Funcs = struct

  let unwrap_float (v : value) =
    match v with
    | VFloat x -> x
    | _ -> failwith "This cannot occur - matrix.ml"

  let unwrap_matrix (v : value) =
    match v with
    | VMatrix x -> x
    | _ -> failwith "This cannot occur - matrix.ml"

  let make_matrix_helper (x : int) (y : int) =
    Array.make_matrix x y 0.0

  let make_matrix (v : value list) =
    let x = List.nth v 0 |> unwrap_float |> int_of_float in 
    let y = List.nth v 1 |> unwrap_float |> int_of_float in 
    VMatrix (make_matrix_helper x y)

  let add_matrix_helper (x1:float array array) (y1:float array array) = 
    let x_row_length = Array.length x1 in 
    let x_column_length = Array.length (Array.get x1 0) in 
    if (x_row_length <> (Array.length y1) ||
        x_column_length <> (Array.length (Array.get y1 0) )) then
      failwith "Invalid input - matrices must be the same size" else
      let new_matrix = make_matrix_helper x_row_length x_column_length in 
      let () = 
        for i = 0 to (x_row_length - 1) do 
          for j = 0 to (x_column_length - 1) do
            new_matrix.(i).(j) <- x1.(i).(j) +. y1.(i).(j)
          done
        done in new_matrix

  let add_matrix (v : value list) =
    let x = List.nth v 0 |> unwrap_matrix in
    let y = List.nth v 1 |> unwrap_matrix  in
    VMatrix (add_matrix_helper x y)


  let sub_matrix_helper (x1:float array array) (y1:float array array) = 
    let x_row_length = Array.length x1 in 
    let x_column_length = Array.length (Array.get x1 0) in 
    if (x_row_length <> (Array.length y1) ||
        x_column_length <> (Array.length (Array.get y1 0) )) then
      failwith "Invalid input - matrices must be the same size" else
      let new_matrix = make_matrix_helper x_row_length x_column_length in 
      let () = 
        for i = 0 to (x_row_length - 1) do 
          for j = 0 to (x_column_length - 1) do
            new_matrix.(i).(j) <- x1.(i).(j) -. y1.(i).(j)
          done
        done in new_matrix

  let sub_matrix (v : value list) = 
    let x1 = List.nth v 0 |> unwrap_matrix in
    let y1 = List.nth v 0 |> unwrap_matrix in
    VMatrix (sub_matrix_helper x1 y1)

  let rec helper_sum_product row_product col_product acc= 
    match acc with 
    |0 -> 0.
    |_ ->(row_product.(acc-1)*.col_product.(acc-1)) +. (helper_sum_product row_product col_product (acc-1))


  let helper_extract row_num col_num matr1 matr2 = 
    let row_product = Array.get matr1 row_num in 
    let col_product = Array.make (Array.length row_product) 0. in
    let () = for i = 0 to (Array.length row_product) - 1 do 
        col_product.(i) <- matr2.(i).(col_num)
      done in 
    helper_sum_product row_product col_product ((Array.length row_product))


  let dot_product_matrix_helper (x1:float array array) (y1:float array array) = 
    let x_row_length = Array.length x1 in 
    let x_column_length = Array.length (Array.get x1 0) in 
    let y_column_length = Array.length (Array.get y1 0) in 
    if (x_column_length <> (Array.length y1)) then
      failwith "Invalid input - matrices must be the same size" else
      let new_matrix = make_matrix_helper x_row_length y_column_length in 
      let () = 
        for i = 0 to (x_row_length - 1) do 
          for j = 0 to (x_column_length - 1) do
            new_matrix.(i).(j) <- helper_extract i j x1 y1 
          done
        done in new_matrix

  let dot_product_matrix (v : value list) = 
    let x1 = List.nth v 0 |> unwrap_matrix in
    let x2 = List.nth v 0 |> unwrap_matrix in
    VMatrix (dot_product_matrix_helper x1 x2)

  let echelon_form_helper x1 = 
    let row_num = (Array.length x1) -1 in
    let col_num = (Array.length (Array.get x1 0))-1 in
    let () = 
      for i = 0 to row_num do 
        for r = i+1 to row_num do 
          let factor = x1.(r).(i) /. x1.(i).(i) in 
          for c = i to col_num do
            x1.(r).(c) <- (x1.(r).(c) -. x1.(i).(c)*.factor);
          done;
          for c = i to col_num do
            if x1.(r).(i) > 0.1 || x1.(r).(i) < -0.1 then 
              x1.(r).(c) <- (x1.(r).(c) /. x1.(r).(i))
            else ()
          done ;
        done ;
      done in 
    x1 
(*
  let echelon_form_helper x1 = 
    let row_num = Array.length x1 in 
    let col_num = Array.length (Array.get x1 0) in
    let () = for i = 0 to row_num-1 do 
        for r = i+1 to row_num-1 do 
          let factor = x1.(i).(i) /. x1.(r).(i) in 
          for c = i to col_num-1 do 
            x1.(r).(c) <- (x1.(r).(c)*.factor) -. x1.(i).(c) 
          done ;
          for c = i to col_num-1 do 
            x1.(r).(c) <- x1.(r).(c) /. x1.(r).(i) 
          done 
        done 
      done in 
    x1 
*)
  let echelon_form v = 
    let x1 = List.nth v 0 |> unwrap_matrix in
    VMatrix (echelon_form_helper x1 )
end

module Matrix_CFU : CFU_sig = struct
  let operation_list = [
    ("madd", Matrix_Functions.add_matrix);
    ("msub", Matrix_Functions.sub_matrix);
    ("mdot", Matrix_Functions.dot_product_matrix);
    ("echelon", Matrix_Functions.echelon_form);
  ]

end