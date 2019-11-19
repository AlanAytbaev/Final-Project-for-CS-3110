module type Statistics_Funcs = sig
  val mean : float list -> float
  val median : float list -> float
  val standard_deviation : float list -> float
  val range : float list -> float
  val min : float list -> float
  val max : float list -> float
  val mean_deviation : float list -> float
  val permutations : float -> float -> float
  val combinations : float -> float -> float
  val factorial : float -> float
end

module Statistics_Functions : Statistics_Funcs = struct

  let mean (s : float list) =
    Float.div (List.fold_left (fun acc x -> acc +. x) 0 s) (List.length s)

  let median (s : float list) =
    let len = List.length s in
    ((List.nth s ((len-1)/2) + List.nth s (len/2))/. 2.)

  let standard_deviation (s : float list) =
    let m = (mean s) in
    s |> List.map (fun x -> (x -. m) *. (x -. m)) |> mean |> Float.sqrt

  let rec min (s : float list) =
    match s with
    | hd::[] -> hd
    | hd::hd2::tl ->
      if (hd > hd2) then min hd2::tl
      else min hd1::tl

  let max (s : float list) =
    match s with
    | hd::[] -> hd
    | hd::hd2::tl ->
      if (hd < hd2) then min hd2::tl
      else hd1:: tl

  let range (s : float list) =
    ((max s) -. (min s))

  let mean_deviation (s : float list) =
  let m = (mean s) in
  s |> List.map (fun x -> Float.abs (x -. m)) |> mean

  let rec factorial (n : float) =
    match n with
    | 0 -> 1
    | _ -> n *. (factorial (n -. 1))

  let permutations (n : float) (r : float) =
    (factorial n) /. (factorial (n -. r))

  let combinations (n : float) (r : float) =
    (factorial n) /. ((factorial (n -. r)) *. (factorial r))
end

module type Statistics_CFU = struct
