open Alcotest

let form_cases cases f =
  let str_case_args ((a, b), _) =
    "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"
  in
  let check_case (args, expected) =
    let result () = (check int) "Wrong answer!" expected (f args) in
    result
  in
  let form_case case =
    test_case (str_case_args case) `Quick (check_case case)
  in
  List.map form_case cases

let form_suit cases (title, solution) = (title, form_cases cases solution)

let form_suits cases headings solutions =
  List.combine headings solutions |> List.map (form_suit cases)
