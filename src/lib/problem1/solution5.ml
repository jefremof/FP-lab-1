open Common

let largest_palindrome lower upper =
  assert (upper > lower && lower > 0);
  let champ = ref (-1) in
  for i = upper downto lower do
    for j = i downto lower do
      let product = i * j in
      if is_palindrome product then champ := max !champ product
    done
  done;
  assert (!champ > 0);
  !champ

let run (lower, upper) = largest_palindrome lower upper
