open Common

(* A sequence of type 'a Seq.t can be thought of as a delayed list, that is,
   a list whose elements are computed only when they are demanded by a consumer.
   This allows sequences to be produced and transformed lazily (one element at a time)
   rather than eagerly (all elements at once).

   This also allows constructing conceptually infinite sequences. *)

let generate_products dif lower =
  let range = Seq.init dif (fun i -> lower + i) in
  let multiply i = Seq.map (fun j -> i * j) range in
  Seq.flat_map multiply range

let largest_palindrome upper lower =
  generate_products (upper - lower + 1) lower
  |> Seq.filter is_palindrome |> Seq.fold_left max (-1)

let run (lower, upper) =
  assert (upper > lower && lower > 0);
  let result = largest_palindrome upper lower in
  assert (result > 0);
  result
