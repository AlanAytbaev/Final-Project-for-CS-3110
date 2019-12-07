open Ast

(** An abstract module type that is meant to structure the CFU modules (ie. Arit    metic functions, calculus functions, statistics functions). A module that ma    tches [CFU_sig] is suitable for use in the [Calc] module. *)
module type CFU_sig = sig
  val operation_list : (string * ( value list -> value)) list
end

module type Set_Funcs = sig
  val intersect : value list -> value
  val union : value list -> value
  val difference : value list -> value
end

module Set_Functions : Set_Funcs = struct

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

  let rec helper short long acc = 
    match acc with 
    |0 -> []
    |i -> let v = long.(i-1) in if (Array.mem v short) then v :: helper short long (acc -1 ) else  helper short long (acc -1 )

  let intersect_helper s1 s2 = 
    let shortest_ar = if ((Array.length s1) > (Array.length s2)) then 
        (s2) else (s1) in 
    let longest_ar = if ((Array.length s1) > (Array.length s2)) then 
        (s1) else (s2) in 
    helper' (helper shortest_ar longest_ar (Array.length longest_ar))

  let intersect (v : value list) =
    let s1 = List.nth v 0 |> unwrap_row in 
    let s2 = List.nth v 1 |> unwrap_row in
    VRow (intersect_helper s1 s2)

  let difference_helper s1 s2 = 
    failwith "unimplemented"

  let difference v = 
    let s1 = List.nth v 0 |> unwrap_row in 
    let s2 = List.nth v 1 |> unwrap_row in
    VRow (difference_helper s1 s2)

  let union_helper s1 s2 = 
    failwith "unimplemented"

  let union v = 
    let s1 = List.nth v 0 |> unwrap_row in 
    let s2 = List.nth v 1 |> unwrap_row in
    VRow (union_helper s1 s2)

end

module Set_CFU  = struct

  let operation_list = [
    ("intersect", Set_Functions.intersect);
    ("difference", Set_Functions.difference);
    ("union", Set_Functions.union);
  ]

end