open Common

let generate_products limitA limitB =
  let modulo_range lim = List.init ((lim * 2) + 1) (fun i -> i - lim) in
  let parity_check b a = b mod 2 <> a mod 2 in
  let combine b =
    modulo_range limitA
    |> List.filter (parity_check b)
    |> List.map (fun a -> (count_primes a b, a * b))
  in
  modulo_range limitB |> List.filter is_prime |> List.map combine
  |> List.flatten

let max_product lower upper =
  let select_champ x y = if fst x > fst y then x else y in
  generate_products lower upper |> List.fold_left select_champ (0, 0) |> snd

let run (limitA, limitB) = max_product limitA limitB
