open OUnit2
open Ast
open Main

(** [make_f n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [float i]. *)
let make_f n i s =
  n >:: (fun _ -> assert_equal (string_of_float i) (fst (Main.interp s Main.initial_env)))


let make_b n i s =
  n >:: (fun _ -> assert_equal (string_of_bool i) (fst (Main.interp s Main.initial_env)))


let make_s n i s =
  n >:: (fun _ -> assert_equal (i) (fst (Main.interp s Main.initial_env)))



let tests = [
  make_f "float" 22. "22";
  make_f "add" 22. "11+11";
  make_f "mul1" 22. "2*11";
  make_f "mul2" 22. "2+2*10";
  make_f "mul3" 14. "2*2+10";
  make_f "mul4" 40. "2*2*10";
  make_f "sub" 23. "30 sub 7";
  make_f "div" 4. "16 / 4";
  make_f "modulus" 6. "13 % 7";
  make_f "modulus" 0. "30 % 3";
  make_f "exponent" 25. "5^2";
  make_f "exponent" 9. "9^1";
  make_f "exponent" 4. "16^0.5";
  make_f "log" 3. "8 log 2";
  make_f "log" 2. "100 log 10";
  make_f "sin" 0. "sin 0";
  make_f "sin" 1. "sin 90";
  make_f "sin" 0.5 "sin 30";
  make_f "cos" 1. "cos 0";
  make_f "cos" 0. "cos 90";
  make_f "cos" 0.5 "cos 60";
  make_f "tan" 0. "tan 0";
  make_f "tan" 1. "tan 45";
  make_f "tan " 0. "tan 180";
  (* make_f "tan" 1. "tan 90"; - MAKE TESTS TO TEST THAT THEY PRODUCE UNDEFINED CORRECTLY *)
  make_f "csc" 2. "csc 30";
  make_f "csc" 1. "csc 90";
  make_f "sec" 1. "sec 0";
  make_f "sec" 2. "sec 60";
  make_f "cot" 1. "cot 45";
  make_f "cot" 0. "cot 90";

  make_b "if then else" true "if 1 then true else false";
  make_b "if then else" false "if 0 then true else false";
  make_b "if then else" true "if 1==1 then true else false";
  make_b "if then else" false "if 1==9 then true else false";

  make_s "tan - undefined" "undefined" "tan 90";
  make_s "tan - undefined" "undefined" "tan 270";
  make_s "csc - undefined" "undefined" "csc 0";
  make_s "csc - undefined" "undefined" "csc 180";
  make_s "sec - undefined" "undefined" "sec 90";
  make_s "sec - undefined" "undefined" "sec 270";
  make_s "cot - undefined" "undefined" "cot 0";
  make_s "cot - undefined" "undefined" "cot 180";

  make_f "stats - mean" 3. "mean 1 2 3 4 5";
  make_f "stats - mean" 1. "mean 1 ";
  make_f "stats - mean" 2. "mean 2 2";
  make_f "stats - mean" 1.5 "mean 3 0";
  make_f "stats - median" 3. "median 1 2 3 4 5";
  make_f "stats - median" 3. "median 2 4 5 1 3";
  make_f "stats - median" 3.5 "median 1 2 3 4 5 6";
  make_f "stats - median" 3.5 "median 6 3 4 2 1 5";
  make_f "stats - median" 3. "median 3";
  make_f "stats - max" 70. "max 0 1 2 3 4 70";
  make_f "stats - max" 8. "max 8 3 2";
  make_f "stats - max" 5. "max 5";
  make_f "stats - min" 5. "min 5";
  make_f "stats - min" 1. "min 1 2 3 4 5";
  make_f "stats - min" 0. "min 4 5 8 2 9 4 9000 0";
  make_f "stats - stddev" 2. "stdev 1 5";
  make_f "stats - stddev" 1. "stdev 2 4";
  make_f "stats - stddev" 1. "stdev 2 2 4 4 ";



]

let suite = "calc test suite" >::: tests

let _ = run_test_tt_main suite
