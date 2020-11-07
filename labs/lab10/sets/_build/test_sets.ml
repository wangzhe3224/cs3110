open OUnit2
open Sets.ListSet

let tests = "test suite for ListSet" >::: [
  "test_empty" >:: (fun _ -> assert_equal [] (to_list empty));
  (* add more black box tests here *)
]

let _ = run_test_tt_main tests