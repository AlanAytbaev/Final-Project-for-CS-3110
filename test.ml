open OUnit2
open Ast
open Main

(** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [Float i]. *)
let make_i n i s =
  n >:: (fun _ -> assert_equal i (fst (Main.interp s Main.initial_env)) ~printer:(fun x-> x))

(* Add Tests Below Here *)
let tests = [
  make_i "float" "22." "22";
  make_i "add" "22." "11+11";
  make_i "mul1" "22." "2*11";
  make_i "mul2" "22." "2+2*10";
  make_i "mul3" "14." "2*2+10";
  make_i "mul4" "40." "2*2*10";
  make_i "let_definition_simple" "5." "let x = 5";
  make_i "let_expression" "0." "let x = 5 in 0";
  make_i "let_expression_complex" "4." "let a = let b = 4 in b in a";
  make_i "let_expression_complex2" "4." "let a = let b = 4 in b in let b = a in b";
  make_i "if_expression1" "1." "if true then 1 else 2";
  make_i "if_expression1" "2." "if false then 1 else 2";
  make_i "if_expression1" "1." "if true then if false then 2 else 1 else 2";
  make_i "boolean" "true" "true";
  make_i "div" "4." "16 / 4";
  make_i "modulus" "6." "13 % 7";
  make_i "modulus" "0." "30 % 3";
  make_i "exponent" "25." "5^2";
  make_i "exponent" "9." "9^1";
  make_i "exponent" "4." "16^0.5";
  make_i "log" "3." "8 log 2";
  make_i "log" "2." "100 log 10";
  make_i "sin" "0." "sin 0";
  make_i "sin" "1." "sin 90";
  make_i "sin" "0.5" "sin 30";
  make_i "cos" "1." "cos 0";
  make_i "cos" "0." "cos 90";
  make_i "cos" "0.5" "cos 60";
  make_i "tan" "0." "tan 0";
  make_i "tan" "1." "tan 45";
  make_i "tan " "0." "tan 180";
  make_i "csc" "2." "csc 30";
  make_i "csc" "1." "csc 90";
  make_i "sec" "1." "sec 0";
  make_i "sec" "2." "sec 60";
  make_i "cot" "1." "cot 45";
  make_i "cot" "0." "cot 90";

  make_i "if then else" "true" "if 1 then true else false";
  make_i "if then else" "false" "if 0 then true else false";
  make_i "if then else" "true" "if 1==1 then true else false";
  make_i "if then else" "false" "if 1==9 then true else false";

  make_i "tan - undefined" "undefined" "tan 90";
  make_i "tan - undefined" "undefined" "tan 270";
  make_i "csc - undefined" "undefined" "csc 0";
  make_i "csc - undefined" "undefined" "csc 180";
  make_i "sec - undefined" "undefined" "sec 90";
  make_i "sec - undefined" "undefined" "sec 270";
  make_i "cot - undefined" "undefined" "cot 0";
  make_i "cot - undefined" "undefined" "cot 180";
  make_i "stats - mean" "3." "mean 1 2 3 4 5";
  make_i "stats - mean" "1." "mean 1 ";
  make_i "stats - mean" "2." "mean 2 2";
  make_i "stats - mean" "1.5" "mean 3 0";
  make_i "stats - median" "3." "median 1 2 3 4 5";
  make_i "stats - median" "3." "median 2 4 5 1 3";
  make_i "stats - median" "3.5" "median 1 2 3 4 5 6";
  make_i "stats - median" "3.5" "median 6 3 4 2 1 5";
  make_i "stats - median" "3." "median 3";
  make_i "stats - max" "70." "max 0 1 2 3 4 70";
  make_i "stats - max" "8." "max 8 3 2";
  make_i "stats - max" "5." "max 5";
  make_i "stats - min" "5." "min 5";
  make_i "stats - min" "1." "min 1 2 3 4 5";
  make_i "stats - min" "0." "min 4 5 8 2 9 4 9000 0";
  make_i "stats - stddev" "2." "stdev 1 5";
  make_i "stats - stddev" "1." "stdev 2 4";
  make_i "stats - stddev" "1." "stdev 2 2 4 4 ";
]

let suite = "calc test suite" >::: tests

let _ = run_test_tt_main suite