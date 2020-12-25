let path = "./bin/input/input_day_22";
let testPath = "./bin/input/input_day_22_test";

let rec play = (xs, ys) => {
  switch (xs, ys) {
  | ([], _) => ys
  | (_, []) => xs
  | ([x, ...xs'], [y, ...ys']) =>
    x > y
      ? play(List.concat([xs', [x, y]]), ys')
      : play(xs', List.concat([ys', [y, x]]))
  };
};

let copyN = (xs, n) => {
  let rec aux = (xs, ys, n) => {
    switch (n) {
    | 0 => ys
    | n => aux(List.tl(xs), List.cons(List.hd(xs), ys), n - 1)
    };
  };
  aux(xs, [], n) |> List.rev;
};

module StringSet = Set.Make(String);

let toString = xs => xs |> List.map(string_of_int) |> String.concat(",");

let rec playRecursive = (xs, ys, lookup) => {
  let state = toString(xs) ++ "|" ++ toString(ys);
  StringSet.mem(state, lookup)
    ? (true, xs)
    : {
      let lookup = StringSet.add(state, lookup);
      switch (xs, ys) {
      | ([], _) => (false, ys)
      | (_, []) => (true, xs)
      | ([x, ...xs'], [y, ...ys']) =>
        if (List.length(xs') >= x && List.length(ys') >= y) {
          let xs'' = copyN(xs', x);
          let ys'' = copyN(ys', y);
          let (didPlayer1Win, _) =
            playRecursive(xs'', ys'', StringSet.empty);
          didPlayer1Win
            ? playRecursive(List.concat([xs', [x, y]]), ys', lookup)
            : playRecursive(xs', List.concat([ys', [y, x]]), lookup);
        } else {
          x > y
            ? playRecursive(List.concat([xs', [x, y]]), ys', lookup)
            : playRecursive(xs', List.concat([ys', [y, x]]), lookup);
        }
      };
    };
};

let run = () => {
  print_endline("---------- Day 22 ----------");
  let (player1, player2, _) =
    Util.getLinesFromFile(path)
    |> List.fold_left(
         ((p1, p2, b), line) =>
           if (b) {
             if (line == "") {
               (p1, p2, false);
             } else {
               (List.cons(int_of_string_opt(line), p1), p2, true);
             };
           } else {
             (p1, List.cons(int_of_string_opt(line), p2), false);
           },
         ([], [], true),
       );
  let (player2, player1) = (List.tl(player1), List.tl(player2));
  let player1' = player1 |> List.filter_map(x => x);
  let player2' = player2 |> List.filter_map(x => x);

  let total = List.length(player1') + List.length(player2');

  let winner =
    play(player1', player2')
    |> List.mapi((i, n) => n * (total - i))
    |> List.fold_left((+), 0);

  Console.log("Part 1> " ++ string_of_int(winner));

  let winner =
    playRecursive(player1', player2', StringSet.empty)
    |> (((_, xs)) => xs)
    |> List.mapi((i, n) => n * (total - i))
    |> List.fold_left((+), 0);
  Console.log("Part 2> " ++ string_of_int(winner));
};