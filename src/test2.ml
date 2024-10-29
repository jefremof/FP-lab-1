open Alcotest
open Fp_lab_1.Samples
open Testing

let () =
  run "Test problem 2" (form_suits problem2_cases titles problem2_solutions)
