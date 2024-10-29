let is_palindrome n =
  let rec helper n tail =
    if n = 0 then tail
    else (helper [@tailcall]) (n / 10) ((tail * 10) + (n mod 10))
  in
  n == helper n 0
