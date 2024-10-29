open Common

let find_max_product limitA limitB =
  let max_product = ref 0 in
  let max_count = ref 0 in
  for b = -limitB to limitB do
    if is_prime b then
      for a = -limitA to limitA do
        if b mod 2 <> a mod 2 then
          let count = count_primes a b in
          if count > !max_count then (
            max_product := a * b;
            max_count := count)
      done
  done;
  assert (!max_count > 0);
  !max_product

let run (limitA, limitB) = find_max_product limitA limitB
