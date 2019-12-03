open OUnit2
open Ast
open Main

(** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [Float i]. *)
let make_i n i s =
  n >:: (fun _ -> assert_equal i (fst (Main.interp s Env.empty)) ~printer:(fun x-> x))

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
]

let suite = "calc test suite" >::: tests

let _ = run_test_tt_main suite