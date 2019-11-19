(** An abstract module type that is meant to structure the CFU modules (ie. Arit    metic functions, calculus functions, statistics functions). A module that ma    tches [CFU_sig] is suitable for use in the [Calc] module. *)
module type CFU_sig = sig

  (** type primitive is the type of the value that the calculator works wi            th*)
  type primitive

  (** An [operation_list] is an association list that maps operation symbo            ls to functions *)
  val operation_list : (string * ( primitive  list -> primitive )) list

  (** [find s] is the operation that is associated with [s] in the operation
      list *)
  val find_function : string -> (float list -> float)
end


(** A module that implements the functions needed for the trigonometric cfu. A module
    that matches [Trigonometric_Funcs] is suitable for use in [Trigonometric_CFU]. *)
module type Trigonometric_Funcs = sig
  val deg_to_rad : float list -> float
  val rad_to_deg : float list -> float
  val sin : float list -> float
  val cos : float list -> float
  val tan : float list -> float
  val sec : float list -> float
  val cosec : float list -> float
  val cotan : float list -> float
end

module Trigonometric_Functions : Trigonometric_Funcs = struct

  let deg_to_rad (f : float list) =
    match f with
    | hd :: tl -> (Float.pi *. hd)/. 180.
    | _ -> failwith "InvalidInput"

  let rad_to_deg (f : float list) =
    match f with
    | hd :: tl -> Float.div (hd |> Float.mul 180.) Float.pi
    | _ -> failwith "InvalidInput"

  let sin (f : float list) =
    match f with
    | hd :: tl -> if (int_of_float hd mod 180 = 0) then 0. else
        let x = deg_to_rad [hd] in
        Float.sin x
    | _ -> failwith "InvalidInput"

  let cos (f : float list) =
    match f with
    | hd :: tl -> if (int_of_float hd mod 90 = 0 && int_of_float hd mod 180 <> 0) then 0.
      else let x = deg_to_rad [hd] in
        Float.cos x
    | _ -> failwith "InvalidInput"

  let tan (f : float list) =
    match f with
    | hd :: tl -> if (int_of_float hd mod 180 = 0) then 0. else
        let x = deg_to_rad [hd] in
        Float.tan x
    | _ -> failwith "InvalidInput"

  let sec (f : float list) =
    match f with
    | hd :: tl -> if (int_of_float hd mod 90 = 0 && int_of_float hd mod 180 <> 0) then failwith "undefined" 
      else let x = deg_to_rad [hd] in
        Float.div 1. (Float.cos x)
    | _ -> failwith "InvalidInput"

  let cosec (f : float list) =
    match f with
    | hd :: tl -> if (int_of_float hd mod 180 = 0 ) then failwith "undefined"
      else let x = deg_to_rad [hd] in
        Float.div 1. (Float.sin x)
    | _ -> failwith "InvalidInput"

  let cotan (f : float list) =
    match f with
    | hd :: tl -> if (int_of_float hd mod 90 = 0 && int_of_float hd mod 180 <> 0) then failwith "undefined" 
      else let x = deg_to_rad [hd] in
        Float.div 1. (Float.tan x)
    | _ -> failwith "InvalidInput"
end

module Trigonometric_CFU : CFU_sig = struct

  type primitive = float

  let operation_list = [
    ("deg", Trigonometric_Functions.deg_to_rad);
    ("rad", Trigonometric_Functions.rad_to_deg);
    ("sin", Trigonometric_Functions.sin);
    ("cos", Trigonometric_Functions.cos);
    ("tan", Trigonometric_Functions.tan);
    ("sec", Trigonometric_Functions.sec);
    ("csc", Trigonometric_Functions.cosec);
    ("cot", Trigonometric_Functions.cotan);
  ]

  let find_function (identifier : string) =
    match List.assoc_opt identifier operation_list with
    |Some f -> f
    |None -> failwith (identifier^" is not a valid imported function")

end
