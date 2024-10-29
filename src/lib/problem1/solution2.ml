open Common

let largest_palindrome lower upper =
  let rec helper i j border acc =
    match (i < lower, j <= border) with
    | true, _ -> acc
    | _, true -> helper (i - 1) (i - 1) j acc
    | _, false ->
        let product = i * j in
        if is_palindrome product then helper (i - 1) (i - 1) j (max product acc)
        else helper i (j - 1) border acc
  in
  helper upper upper lower (-1)

let run (lower, upper) =
  assert (upper > lower && lower > 0);
  let result = largest_palindrome lower upper in
  assert (result > 0);
  result
