open Dynan
open Testing
open Arg
open Check

exception Ran_tests
(* let suite = ref (Studenttests.provided_tests @ Gradedtests.graded_tests) *)

let test = test_oc 'a' 'f' ~test_number:100 ~strings_number:10  ~string_size:4 ~rel_size:7 ~rep:4

(* let x = QCheck.Gen.generate ~n:10 @@ Qgen.gen 'a' 'a' ~strings_number:10  ~strings:7 ~rel_size:1 ~rep:5 ~pattern:["O"; "C"] *)

let execute_tests () =
  let _ = QCheck_runner.run_tests ~verbose:true [test] in
  (* List.iter (fun w -> Printf.printf "%s%!" (Qgen.print_test w)) x; *)
  (* Printf.printf "%b = %b" b b'; *)
  raise Ran_tests
    
let args =
  [
    ("--test", Unit execute_tests, "run the test suite, ignoring other files inputs")
  ] 

let files = ref []

let _ =
  try
    Arg.parse args (fun filename -> files := filename :: !files)
      "Main test harness\n\
       USAGE: ./main.native [options] <files>\n\
       see README for details about using the compiler";
  with Ran_tests ->
    ()
