open Common

let generate_products limitA limitB =
  let f (a, b) = if b = limitB then (a + 1, -limitB) else (a, b + 1) in
  let p (a, _) = a <= limitA in
  Iter.iterate f (-limitA, -limitB)
  |> Iter.take_while p
  |> Iter.map (fun (a, b) -> (count_primes a b, a * b))

let max_product lower upper =
  let aux x y = if fst x > fst y then x else y in
  generate_products lower upper |> Iter.fold aux (0, 0) |> snd

let run (limitA, limitB) = max_product limitA limitB
