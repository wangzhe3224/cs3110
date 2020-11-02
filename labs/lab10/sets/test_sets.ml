open OUnit2
open Sets.ListSet

let tests = "test suite for ListSet" >::: [
  "test_empty" >:: (fun _ -> assert_equal [] (to_list empty));
  (* add more black box tests here *)
  "test_size_of_empty" >:: (fun _ -> assert_equal 0 (size empty))
  "test_size_insert" >:: (fun _ -> 1 (insert 1 empty))
]

let _ = run_test_tt_main tests
