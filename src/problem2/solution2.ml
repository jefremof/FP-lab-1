open Common

let max_product limitA limitB =
  let rec helper count product a b =
    let parity_check = b mod 2 <> a mod 2 in
    let suitable = is_prime b && a <= limitA && parity_check in
    if b > limitB then product
    else if suitable then
      let new_count = count_primes a b in
      if new_count > count then helper new_count (a * b) (a + 1) b
      else helper count product (a + 1) b
    else helper count product (-limitA) (b + 1)
  in
  helper 0 0 (-limitA) (-limitB)

let run (limitA, limitB) = max_product limitA limitB
