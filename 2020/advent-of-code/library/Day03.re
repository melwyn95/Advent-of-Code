let path = "./bin/input/input_day_3";
let testPath = "./bin/input/input_day_3_test";

let lines =
  Util.getLinesFromFile(path)
  |> List.map(Util.stringToCharList)
  |> List.rev;

let rows = lines |> List.length;
let cols = lines |> List.hd |> List.length;

let getIndexes = (times, rowOffset, colOffset) => {
  let rec aux = (acc, prev, n) => {
    switch (n, prev) {
    | (0, _) => acc
    | (_, (x, y)) =>
      aux(
        List.cons((x + rowOffset, y + colOffset), acc),
        (x + rowOffset, y + colOffset),
        n - 1,
      )
    };
  };
  aux([], (0, 0), times);
};

let findTrees = indexes => {
  indexes
  |> List.map(((c, r)) => List.nth(List.nth(lines, r), c mod cols))
  |> List.filter(chr => chr === '#')
  |> List.length;
};

let run = () => {
  print_endline("---------- Day 3 ----------");

  let indexes = getIndexes(rows - 1, 3, 1);

  indexes
  |> List.filter(((_, c)) => c <= rows - 1)
  |> findTrees
  |> (n => Console.log("Part 1> " ++ string_of_int(n)));

  let offsets = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];
  offsets
  |> List.map(((r, c)) =>
       getIndexes(rows - 1, r, c) |> List.filter(((_, c)) => c <= rows - 1)
     )
  |> List.map(findTrees)
  |> List.fold_left(( * ), 1)
  |> (n => Console.log("Part 2> " ++ string_of_int(n)));
};