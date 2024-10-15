open Common

let generate_products diff lower =
  let range = List.init diff (fun i -> lower + i) in
  let multiply i = List.map (fun j -> i * j) range in
  List.flatten (List.map multiply range)

let largest_palindrome upper lower =
  generate_products (upper - lower + 1) lower
  |> List.filter is_palindrome |> List.fold_left max (-1)

let run (lower, upper) =
  assert (upper > lower && lower > 0);
  let result = largest_palindrome upper lower in
  assert (result > 0);
  result
