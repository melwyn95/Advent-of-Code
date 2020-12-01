// print_endline("Running Test Program:");
// let () = print_endline(Library.Util.hello());

print_endline(Sys.getcwd());
let ic = open_in("./bin/input_day_1");

let rec getIntsFromFile = (ic, ints) =>
  try(
    input_line(ic)
    |> int_of_string
    |> (x => List.cons(x, ints) |> getIntsFromFile(ic))
  ) {
  | End_of_file =>
    close_in(ic);
    ints;
  };

let numbers = getIntsFromFile(ic, []);

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

switch (findSum2020(numbers)) {
| Some((x, y)) =>
  print_int(x * y);
  print_newline();
| None => print_endline("No Answer")
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

switch (newFindSum2020(numbers)) {
| Some((x, y, z)) =>
  print_int(x * y * z);
  print_newline();
| None => print_endline("No Answer")
};