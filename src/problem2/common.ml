let is_prime n =
  let rec helper d =
    if d * d > n then true else if n mod d = 0 then false else helper (d + 1)
  in
  n > 1 && helper 2

let count_primes a b =
  let rec helper n count =
    let value = (n * n) + (a * n) + b in
    if is_prime value then helper (n + 1) (count + 1) else count
  in
  helper 0 0
