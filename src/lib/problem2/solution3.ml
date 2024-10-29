open Common

let generate_products limitA limitB =
  let rec helper a b acc =
    match (a > limitA, b > limitB) with
    | true, _ -> acc
    | _, true -> helper (a + 1) (-limitB) acc
    | _, false ->
        let count = count_primes a b in
        helper a (b + 1) ((count, a * b) :: acc)
  in
  helper (-limitA) (-limitB) []

let max_product lower upper =
  let select_champ x y = if fst x > fst y then x else y in
  generate_products lower upper |> List.fold_left select_champ (0, 0) |> snd

let run (limitA, limitB) = max_product limitA limitB
