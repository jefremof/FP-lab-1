let titles =
  [
    "Solution 1 (regular recursion)";
    "Solution 2 (tail recursion)";
    "Solution 3 (modules)";
    "Solution 4 (map-generated)";
    "Solution 5 (loop syntax)";
    "Solution 6 (lazy Seq)";
  ]

let problem1_solutions =
  let open Problem1 in
  [
    Solution1.run;
    Solution2.run;
    Solution3.run;
    Solution4.run;
    Solution5.run;
    Solution6.run;
  ]

let problem2_solutions =
  let open Problem2 in
  [
    Solution1.run;
    Solution2.run;
    Solution3.run;
    Solution4.run;
    Solution5.run;
    Solution6.run;
  ]

let problem1_cases = [ ((100, 999), 906609); ((10, 99), 9009) ]
let problem2_cases = [ ((999, 999), -59231); ((80, 2000), -126479) ]
