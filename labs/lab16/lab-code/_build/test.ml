open OUnit2
open Ast
open Main

(* A few test cases *)
let tests = [
  "int"  >:: (fun _ -> assert_equal (VInt 22) (interp "22"));
  "add"  >:: (fun _ -> assert_equal (VInt 22 ) (interp "11+11"));
  "adds" >:: (fun _ -> assert_equal (VInt 22) (interp "(10+1)+(5+6)"));
  "let"  >:: (fun _ -> assert_equal (VInt 22) (interp "let x=22 in x"));
  "lets" >:: (fun _ -> assert_equal (VInt 22) (interp "let x = 0 in let x = 22 in x"));
  "times1"  >:: (fun _ -> assert_equal (VInt 22 ) (interp "11*2"));
  "times_add"  >:: (fun _ -> assert_equal (VInt 22 ) (interp "10*2+2"));
  "add_times"  >:: (fun _ -> assert_equal (VInt 22 ) (interp "2+10*2"));
  "add_times_add"  >:: (fun _ -> assert_equal (VInt 23 ) (interp "2+10*2+1"));
  "times_times"  >:: (fun _ -> assert_equal (VInt 44 ) (interp "11*2*2"));
  "if true then 22 else 0"  >:: (fun _ -> assert_equal (VInt 22 ) (interp "if true then 22 else 0"));
]

let _ = run_test_tt_main ("suite" >::: tests)

