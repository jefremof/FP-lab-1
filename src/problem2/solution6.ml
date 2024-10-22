open Common

let generate_products limitA limitB =
  let modulo_range lim = Seq.init ((lim * 2) + 1) (fun i -> i - lim) in
  let parity_check b a = b mod 2 <> a mod 2 in
  let combine b =
    modulo_range limitA
    |> Seq.filter (parity_check b)
    |> Seq.map (fun a -> (count_primes a b, a * b))
  in
  modulo_range limitB |> Seq.filter is_prime |> Seq.flat_map combine

let max_product lower upper =
  let aux x y = if fst x > fst y then x else y in
  generate_products lower upper |> Seq.fold_left aux (0, 0) |> snd

let run (limitA, limitB) = max_product limitA limitB
