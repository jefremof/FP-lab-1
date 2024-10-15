open Common

let generate_products lower upper =
  let rec helper i j lst =
    if i < lower then lst
    else if j < lower then helper (i - 1) (i - 1) lst
    else helper i (j - 1) ((i * j) :: lst)
  in
  helper upper upper []

let largest_palindrome upper lower =
  generate_products lower upper
  |> List.filter is_palindrome |> List.fold_left max (-1)

let run (lower, upper) =
  assert (upper > lower && lower > 0);
  let result = largest_palindrome upper lower in
  assert (result > 0);
  result
