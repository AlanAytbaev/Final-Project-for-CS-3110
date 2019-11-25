module type CFU_sig = sig
  type primitive
  val operation_list : (string * ( primitive  list -> primitive )) list
  val find_function : string -> (float list -> float)
end

module type Statistics_Funcs = sig
  val mean : float list -> float
  val median : float list -> float
  val standard_deviation : float list -> float
  val range : float list -> float
  val min : float list -> float
  val max : float list -> float
  val permutations : float list-> float
  val combinations : float list ->  float
  val factorial : float -> float
end

module Statistics_Functions : Statistics_Funcs = struct

  let mean (s : float list) =
    (List.fold_left (fun acc x -> acc +. x) 0. s) /. (float_of_int(List.length s))

  let median (s : float list) =
    let s_new = List.sort compare s  in 
    let len = List.length s_new in 
    let mid = List.nth s_new (len/2) in 
    if (len mod 2<>0) then mid else 
      let mid2 = List.nth s_new ((len-1)/2) in 
      (mid+.mid2)/.2.


  let standard_deviation (s : float list) =
    let m = (mean s) in
    s |> List.map (fun x -> (x -. m) *. (x -. m)) |> mean |> Float.sqrt

  let rec min_helper (s : float list) min_acc=
    match s with
    | [] -> min_acc
    | h::t -> if (h<min_acc) then min_helper t h else min_helper t min_acc 

  let min s = min_helper s max_float

  let rec max_helper  (s : float list) max_acc=
    match s with
    | [] -> max_acc
    | h::t -> if (h>max_acc) then max_helper t h else max_helper t max_acc

  let max s = max_helper s min_float

  let range (s : float list) =
    ((max s) -. (min s))

  let rec factorial (n : float) =
    match n with
    | 0. -> 1.
    | _ -> n *. (factorial (n -. 1.))

  let permutations nr =
    match nr with
    |[] -> failwith "wrong number of arguments"
    |h1::[] -> failwith "wrong number of arguments"
    |n::r::[] -> if (n<r) then failwith "First argument must be greater" 
      else (factorial n) /. (factorial (n -. r))
    |_ -> failwith "wrong number of arguments"

  let combinations nr =
    match nr with
    |[] -> failwith "wrong number of arguments"
    |h1::[] -> failwith "wrong number of arguments"
    |n::r::[] -> if (n<r) then failwith "First argument must be greater" else 
        (factorial n) /. ((factorial (n -. r)) *. (factorial r))
    |_ -> failwith "wrong number of arguments"

end

module Statistics_CFU = struct
  type primitive = float

  let operation_list = [
    ("mean", Statistics_Functions.mean);
    ("median", Statistics_Functions.median);
    ("stdev", Statistics_Functions.standard_deviation);
    ("min", Statistics_Functions.min);
    ("max", Statistics_Functions.max);
    ("range", Statistics_Functions.range);
    ("perm", Statistics_Functions.permutations);
    ("comb", Statistics_Functions.combinations)
  ]
  let find_function (identifier : string) =
    match List.assoc_opt identifier operation_list with
    |Some f -> f
    |None -> failwith (identifier^" is not a valid imported function")

end
