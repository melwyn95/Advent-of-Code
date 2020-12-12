type seat =
  | Empty
  | Occupied
  | Floor;

let of_string = c =>
  switch (c) {
  | 'L' => Empty
  | '#' => Occupied
  | '.'
  | _ => Floor
  };

let to_string = s =>
  switch (s) {
  | Empty => 'L'
  | Occupied => '#'
  | Floor => '.'
  };

let testPath = "./bin/input_day_11_test";
let path = "./bin/input_day_11";

let getCell = (xs, r, c) => xs |> Array.get(_, r) |> Array.get(_, c);

let getNeighbours = (rowLength, colLength, xs, rowIndex, colIndex) => {
  [
    (rowIndex - 1, colIndex - 1),
    (rowIndex - 1, colIndex),
    (rowIndex - 1, colIndex + 1),
    (rowIndex, colIndex - 1),
    (rowIndex, colIndex + 1),
    (rowIndex + 1, colIndex - 1),
    (rowIndex + 1, colIndex),
    (rowIndex + 1, colIndex + 1),
  ]
  |> List.filter(((r, c)) =>
       r >= 0 && r <= rowLength - 1 && c >= 0 && c <= colLength - 1
     )
  |> List.map(((r, c)) => getCell(xs, r, c));
};

let getVisibleCell = (xs, rLimit, cLimit, r, c, rOffset, cOffset) => {
  let rec aux = (r, c) =>
    if (r >= 0 && r <= rLimit - 1 && c >= 0 && c <= cLimit - 1) {
      switch (getCell(xs, r, c)) {
      | Empty => Empty
      | Occupied => Occupied
      | Floor => aux(r + rOffset, c + cOffset)
      };
    } else {
      Floor;
    };
  aux(r, c);
};

let getNeighboursNew = (rowLength, colLength, xs, rowIndex, colIndex) => {
  let getVisibleCell = getVisibleCell(xs, rowLength, colLength);
  let xs = [
    getVisibleCell(rowIndex - 1, colIndex - 1, -1, -1),
    getVisibleCell(rowIndex - 1, colIndex, -1, 0),
    getVisibleCell(rowIndex - 1, colIndex + 1, -1, 1),
    getVisibleCell(rowIndex, colIndex - 1, 0, -1),
    getVisibleCell(rowIndex, colIndex + 1, 0, 1),
    getVisibleCell(rowIndex + 1, colIndex - 1, 1, -1),
    getVisibleCell(rowIndex + 1, colIndex, 1, 0),
    getVisibleCell(rowIndex + 1, colIndex + 1, 1, 1),
  ];
  xs;
};

let print_seats = xs => {
  Array.iter(
    ys => {
      Array.iter(s => s |> to_string |> print_char, ys);
      print_newline();
    },
    xs,
  );
};

let compare = (oldLayout, newLayout) => {
  Array.map2(
    (oldRow, newRow) => Array.map2((s1, s2) => s1 == s2, oldRow, newRow),
    oldLayout,
    newLayout,
  )
  |> Array.for_all(row => Array.for_all(x => x, row));
};

let rec nextLayout = (oldLayout, occupiedLimit, getNeighbours) => {
  let newLayout =
    oldLayout
    |> Array.mapi((rowIndex, row) =>
         row
         |> Array.mapi((colIndex, seat) => {
              let numberOfOccupiedAdjSeats =
                getNeighbours(oldLayout, rowIndex, colIndex)
                |> List.filter(s => s == Occupied)
                |> List.length;
              switch (seat) {
              | Empty => numberOfOccupiedAdjSeats == 0 ? Occupied : Empty
              | Occupied =>
                numberOfOccupiedAdjSeats >= occupiedLimit ? Empty : Occupied
              | Floor => Floor
              };
            })
       );

  if (compare(oldLayout, newLayout)) {
    newLayout;
  } else {
    nextLayout(newLayout, occupiedLimit, getNeighbours);
  };
};

let run = () => {
  print_endline("---------- Day 11 ----------");
  let originalLayout =
    Util.getLinesFromFile(path)
    |> List.rev
    |> List.map(line =>
         line |> Util.stringToCharList |> List.map(of_string) |> Array.of_list
       )
    |> Array.of_list;

  let rowLength = Array.length(originalLayout);
  let colLength = originalLayout |> Array.get(_, 0) |> Array.length;

  let neighboursFn = getNeighbours(rowLength, colLength);
  let occupied =
    nextLayout(originalLayout, 4, neighboursFn)
    |> Array.fold_left(
         (count, row) =>
           row
           |> Array.fold_left(
                (c, cell) => c + (cell == Occupied ? 1 : 0),
                count,
              ),
         0,
       );

  Console.log("Part 1> " ++ string_of_int(occupied));

  let newNeighboursFn = getNeighboursNew(rowLength, colLength);
  let occupied =
    nextLayout(originalLayout, 5, newNeighboursFn)
    |> Array.fold_left(
         (count, row) =>
           row
           |> Array.fold_left(
                (c, cell) => c + (cell == Occupied ? 1 : 0),
                count,
              ),
         0,
       );
  Console.log("Part 2> " ++ string_of_int(occupied));
};