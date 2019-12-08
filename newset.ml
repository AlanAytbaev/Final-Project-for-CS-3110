open Ast

(** An abstract module type that is meant to structure the CFU modules (ie. Arit
    metic functions, calculus functions, statistics functions). A module that ma 
    tches [CFU_sig] is suitable for use in the [Calc] module. *)

module type CFU_sig = sig
  val operation_list : (string * ( value list -> value)) list
end

module type NewSet_Funcs = sig
  val isEqual : value list -> value
  val isSubset :value list -> value
  val isDisjoint : value list -> value
end

module NewSet_Functions : NewSet_Funcs = struct

  let unwrap_float (v : value) =
    match v with
    | VFloat x -> x
    | _ -> failwith "This cannot occur - matrix.ml"

  let unwrap_row (v : value) : float array =
    match v with
    | VRow x -> x
    | _ -> failwith "This cannot occur - matrix.ml"

  let helper' lst = 
    Array.of_list lst 

  (** [check x y int bool] checks whether every element in x is present in y and
      returns [bool] true if so and returns false if otherwise *)
  let rec check_elements_helper x y int bool= 
    match int with 
    |0 -> bool
    |i -> if (Array.mem x.(i-1) y)
      then check_elements_helper x y (i-1) (true&&bool) 
      else false

  (** [isEqual x y] returns true if two sets contain structurally equal elements 
      and have the same number of elements and returns false otherwise*)
  let isEqual_helper x y =
    if (Array.length x = Array.length y) then 
      check_elements_helper x y (Array.length x) true
    else false

  let isEqual v = 
    let s1 = List.nth v 0 |> unwrap_row in 
    let s2 = List.nth v 1 |> unwrap_row in 
    VBool (isEqual_helper s1 s2)

  (** [isSubset x y] returns true if all elements in [x] is also there in [y] *)
  let isSubset v = 
    let s1 = List.nth v 0 |> unwrap_row in 
    let s2 = List.nth v 1 |> unwrap_row in 
    VBool(check_elements_helper s1 s2 (Array.length s1) true)

  let rec check_elements_helper_2 x y int bool= 
    match int with 
    |0 -> bool
    |_ -> if (Array.mem x.(int-1) y) 
      then  false
      else check_elements_helper x y (int-1) (true&&bool) 

  (** [isDisjoint x y] returns true if none of the elements in [x] are
      in [y] and returns false otherwise*)
  let isDisjoint v = 
    let s1 = List.nth v 0 |> unwrap_row in 
    let s2 = List.nth v 1 |> unwrap_row in 
    VBool (check_elements_helper_2 s1 s2 (Array.length s1) true)

end 

module NewSet_CFU = struct

  let operation_list = [
    ("is_equal", NewSet_Functions.isEqual);
    ("is_disjoint", NewSet_Functions.isDisjoint);
    ("is_subset", NewSet_Functions.isSubset);
  ]

end