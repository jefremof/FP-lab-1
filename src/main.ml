open Fp_lab_1.Samples
open Benchmark

let print_pairs = List.iter (fun (s, n) -> Printf.printf "%s:\n%d\n\n" s n)

let print_answers text borders answers =
  let l, u = borders in
  Printf.printf "%s. %d, %d\n\n" text l u;
  print_pairs (List.combine titles answers)

let benchmark_solutions borders solutions reps =
  let benchmark ((s, r), f) =
    let _ = latency1 ~name:s r f borders in
    print_newline ()
  in
  List.iter benchmark (List.combine (List.combine titles reps) solutions)

let () =
  let borders = (100, 999) in
  let problem1_answers = List.map (( |> ) borders) problem1_solutions in
  print_answers "First problem" borders problem1_answers;
  Printf.printf "\n\n";
  let reps = [ 5000L; 5000L; 100L; 50L; 100L; 100L ] in
  benchmark_solutions borders problem1_solutions reps;
  Printf.printf "\n\n\n"

let () =
  let borders = (999, 999) in
  let problem2_answers = List.map (( |> ) borders) problem2_solutions in
  print_answers "Second problem" borders problem2_answers;
  Printf.printf "\n\n";
  let reps = [ 30L; 30L; 10L; 10L; 30L; 30L ] in
  benchmark_solutions borders problem2_solutions reps;
  Printf.printf "\n\n\n"
