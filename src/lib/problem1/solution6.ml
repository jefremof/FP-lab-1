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

(*
     (* open Common *)

   (* A sequence of type 'a Seq.t can be thought of as a delayed list, that is,
      a list whose elements are computed only when they are demanded by a consumer.
      This allows sequences to be produced and transformed lazily (one element at a time)
      rather than eagerly (all elements at once).

      This also allows constructing conceptually infinite sequences. *)

   (* let create_hashmap () =
     let hashmap = Hashtbl.create 100 in
       for i = 10 to 99 do
         for j = i to 99 do
           Hashtbl.add hashmap (i * j) (is_palindrome (i * j));
         done;
       done;
     hashmap

   let map = create_hashmap () *)

   (* let palindrome n =
     match (Hashtbl.find_opt map n) with
       | Some v -> v
       | None -> false *)

   let generate_products upper lower =
     let range = Seq.ints lower |> Seq.filter (fun i -> i <= upper) in
     let multiply i = Seq.map (fun j -> i * j) (range |> Seq.filter (fun j -> (j mod 11) <> (i mod 11))) in
     Seq.flat_map multiply range

   let largest_palindrome upper lower =
     generate_products (upper - lower + 1) lower
     |> Seq.filter (fun i -> i mod 2 = 0) |> Seq.fold_left max (-1)

   let run (lower, upper) =
     assert (upper > lower && lower > 0);
     let result = largest_palindrome upper lower in
     assert (result > 0);
     result *)
