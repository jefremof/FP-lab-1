open Common

let generate_products limitA limitB =
  let rec helper a b acc =
    if a > limitA then acc
    else if b > limitB then helper (a + 1) (-limitB) acc
    else
      let count = count_primes a b in
      helper a (b + 1) ((count, a * b) :: acc)
  in
  helper (-limitA) (-limitB) []

let max_product lower upper =
  let select_champ x y = if fst x > fst y then x else y in
  generate_products lower upper |> List.fold_left select_champ (0, 0) |> snd

let run (limitA, limitB) = max_product limitA limitB
