open OUnit2
open Ast
open Main
open Arithmetic

(** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [Int i]. *)
let make_i n i s =
  n >:: (fun _ -> assert_equal (string_of_float i) (interp s))

(* Add Tests Below Here *)

let tests = [
  make_i "float" 22. "22";
  make_i "add" 22. "11+11";
  make_i "mul1" 22. "2*11";
  make_i "mul2" 22. "2+2*10";
  make_i "mul3" 14. "2*2+10";
  make_i "mul4" 40. "2*2*10";
]

let suite = "calc test suite" >::: tests

let _ = run_test_tt_main suite
