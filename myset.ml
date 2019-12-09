open Ast


module type CFU_sig = sig
  val operation_list : (string * ( value list -> value)) list
end

module type MySet_Funcs = sig
  val intersect : value list -> value
  val union : value list -> value
  val difference : value list -> value
end

module MySet_Functions : MySet_Funcs = struct

  (** [unwrap_float v] is the float extracted from value [v] *)
  let unwrap_float (v : value) =
    match v with
    | VFloat x -> x
    | _ -> failwith "This cannot occur - matrix.ml"

  (** [unwrap_row v] is the row extracted from value [v] *)
  let unwrap_row (v : value) : float array =
    match v with
    | VRow x -> x
    | _ -> failwith "This cannot occur - matrix.ml"


  (** [helper short long acc] is a list containing the elements present 
      in both [short] and [long] *)
  let rec helper short long acc = 
    match acc with 
    |0 -> []
    |i -> let v = long.(i-1) in 
      if (Array.mem v short) then v :: helper short long (acc -1 ) else 
        helper short long (acc -1 )

  (** [intersect_helper s1 s2] is the array containing the elements present 
        in both [s1] and [s2] *)
  let intersect_helper s1 s2 = 
    let shortest_ar = if ((Array.length s1) > (Array.length s2)) then 
        (s2) else (s1) in 
    let longest_ar = if ((Array.length s1) > (Array.length s2)) then 
        (s1) else (s2) in 
    Array.of_list (helper shortest_ar longest_ar (Array.length longest_ar))


  let intersect (v : value list) =
    let s1 = List.nth v 0 |> unwrap_row in 
    let s2 = List.nth v 1 |> unwrap_row in
    VRow (intersect_helper s1 s2)


  (**  [difference_helper s1 s2 acc] is the list of elements contained in 
       [s1] and not in [s2] *)
  let rec difference_helper s1 s2 acc = 
    match acc with 
    |0 -> []
    |i -> let v = s1.(i-1) in if not (Array.mem v s2) then 
        v :: difference_helper s1 s2 (acc -1 ) else  
        difference_helper s1 s2 (acc -1 )

  let difference v = 
    let s1 = List.nth v 0 |> unwrap_row in 
    let s2 = List.nth v 1 |> unwrap_row in
    let lst = (difference_helper s1 s2 (Array.length s1)) in 
    VRow (Array.of_list lst)


  (** [union_helper s1 s2] is the array with elements contained in both [s1] and 
      [s2] *)
  let union_helper s1 s2 = 
    let l1 = Array.to_list s1 in 
    let l2 = Array.to_list s2 in 
    let l3 = l1 @ l2 in 
    let l4 = List.sort_uniq compare l3 in 
    Array.of_list l4 

  let union v = 
    let s1 = List.nth v 0 |> unwrap_row in  
    let s2 = List.nth v 1 |> unwrap_row in
    VRow (union_helper s1 s2)

end

module MySet_CFU  = struct

  let operation_list = [
    ("intersect", MySet_Functions.intersect);
    ("difference", MySet_Functions.difference);
    ("union", MySet_Functions.union);
  ]

end