open OUnit2
open Sum

let tests = "test suite for sum" >::: [
    "empty" >:: (fun _ -> assert_equal 0 (sum []));
]

let _ = run_test_tt_main tests
