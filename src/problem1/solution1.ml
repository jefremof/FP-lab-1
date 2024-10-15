open Common

let largest_palindrome lower upper =
  let rec helper i j border =
    if i < lower then -1
    else if j <= border then helper (i - 1) (i - 1) j
    else
      let product = i * j in
      if is_palindrome product then max product (helper (i - 1) (i - 1) j)
      else helper i (j - 1) border
  in
  helper upper upper lower

let run (lower, upper) =
  assert (upper > lower && lower > 0);
  let result = largest_palindrome lower upper in
  assert (result > 0);
  result
