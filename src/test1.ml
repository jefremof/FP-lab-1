open Alcotest
open Fp_lab_1.Samples
open Testing

let () =
  run "Test problem 1" (form_suits problem1_cases titles problem1_solutions)
