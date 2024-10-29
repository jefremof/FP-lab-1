open Common

let max_product limitA limitB =
  let rec helper a b =
    let parity_check = b mod 2 <> a mod 2 in
    let suitable = is_prime b && a <= limitA && parity_check in
    if b > limitB then (0, 0)
    else if suitable then
      let count = count_primes a b in
      let further = helper (a + 1) b in
      if count > fst further then (count, a * b) else further
    else helper (-limitA) (b + 1)
  in
  let count, result = helper (-limitA) (-limitB) in
  assert (count > 0);
  result

let run (limitA, limitB) = max_product limitA limitB
