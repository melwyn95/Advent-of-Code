let path = "./bin/input_day_1";
let numbers = Util.getLinesFromFile(path) |> List.map(int_of_string);

/* Part 1 */
let rec findSum2020 = xs =>
  switch (xs) {
  | [] => None
  | [hd, ...tl] =>
    switch (List.find_opt(x => x + hd == 2020, tl)) {
    | None => findSum2020(tl)
    | Some(x) => Some((x, hd))
    }
  };

/* Part 2 */
let rec findSum = (xs, sum) =>
  switch (xs) {
  | [] => None
  | [hd, ...tl] =>
    switch (List.find_opt(x => x + hd == sum, tl)) {
    | None => findSum(tl, sum)
    | Some(x) => Some((x, hd))
    }
  };

let rec newFindSum2020 = xs =>
  switch (xs) {
  | [] => None
  | [hd, ...tl] =>
    switch (findSum(tl, 2020 - hd)) {
    | Some((x, y)) => Some((hd, x, y))
    | None => newFindSum2020(tl)
    }
  };

let run = () => {
  print_endline("---------- Day 1 ----------");
  /* Part 1 */
  switch (findSum2020(numbers)) {
  | Some((x, y)) => print_endline("Part 1> " ++ string_of_int(x * y))
  | None => print_endline("No Answer")
  };
  /* Part 2 */
  switch (newFindSum2020(numbers)) {
  | Some((x, y, z)) => print_endline("Part 2> " ++ string_of_int(x * y * z))
  | None => print_endline("No Answer")
  };
};