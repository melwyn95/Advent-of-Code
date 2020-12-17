let path = "./bin/input/input_day_17";
let testPath = "./bin/input/input_day_17_test";

type t =
  | Active
  | InActive;
let of_char =
  fun
  | '.' => InActive
  | '#' => Active
  | _ => failwith("invaild state");

let pad = (n, cubes) => {
  let rowLength = cubes[0] |> Array.length;
  let empties = Array.make(n, InActive);
  let emptyRow = Array.make(2 * n + rowLength, InActive);
  let emptyRows = Array.make(n, emptyRow);
  let inputRows =
    cubes |> Array.map(row => Array.concat([empties, row, empties]));
  let mainLayer = Array.concat([emptyRows, inputRows, emptyRows]);
  let emptyLayer =
    Array.concat([emptyRows, Array.make(rowLength, emptyRow), emptyRows]);
  let emptyLayers = Array.make(n, emptyLayer) |> Array.to_list;
  emptyLayers |> List.cons(mainLayer) |> List.append(emptyLayers);
};

let getNeighbours = (i, j) => (
  [
    (i - 1, j - 1),
    (i - 1, j),
    (i - 1, j + 1),
    (i, j - 1),
    (i, j),
    (i, j + 1),
    (i + 1, j - 1),
    (i + 1, j),
    (i + 1, j + 1),
  ],
  [
    (i - 1, j - 1),
    (i - 1, j),
    (i - 1, j + 1),
    (i, j - 1),
    (i, j + 1),
    (i + 1, j - 1),
    (i + 1, j),
    (i + 1, j + 1),
  ],
  [
    (i - 1, j - 1),
    (i - 1, j),
    (i - 1, j + 1),
    (i, j - 1),
    (i, j),
    (i, j + 1),
    (i + 1, j - 1),
    (i + 1, j),
    (i + 1, j + 1),
  ],
);

let getActiveNeighboursFromLayer = (indices, layer) => {
  let n = layer[0] |> Array.length;
  indices
  |> List.filter(((r, c)) => !(r < 0 || c < 0 || r == n || c == n))
  |> List.map(((r, c)) => layer[r][c])
  |> List.filter(t =>
       switch (t) {
       | Active => true
       | InActive => false
       }
     );
};

let processLayer = (prev, curr, next) => {
  curr
  |> Array.mapi((r, row) =>
       Array.mapi(
         (c, cell) => {
           let (pN, cN, nN) = getNeighbours(r, c);
           let activeTopLayer =
             getActiveNeighboursFromLayer(pN, prev) |> List.length;
           let activeCurrentLayer =
             getActiveNeighboursFromLayer(cN, curr) |> List.length;
           let activeBottomLayer =
             getActiveNeighboursFromLayer(nN, next) |> List.length;

           let activeNeighbours =
             activeBottomLayer + activeCurrentLayer + activeTopLayer;
           switch (cell) {
           | Active =>
             activeNeighbours == 2 || activeNeighbours == 3 ? Active : InActive
           | InActive => activeNeighbours == 3 ? Active : InActive
           };
         },
         row,
       )
     );
};

let cycle = (layers, n) =>
  layers
  |> List.mapi((i, layer) =>
       switch (i) {
       | 0 => layer
       | x when x == 2 * n => layer
       | _ =>
         processLayer(
           List.nth(layers, i - 1),
           layer,
           List.nth(layers, i + 1),
         )
       }
     );

let times = (n, layers) => {
  let rec aux = (m, layers) =>
    switch (m) {
    | 0 => layers
    | _ => aux(m - 1, cycle(layers, n))
    };
  aux(n, layers);
};

let countActive = layers =>
  layers
  |> List.map(layer =>
       layer
       |> Array.fold_left(
            (acc, row) =>
              row
              |> Array.fold_left(
                   (s, c) =>
                     switch (c) {
                     | Active => s + 1
                     | InActive => s
                     },
                   acc,
                 ),
            0,
          )
     )
  |> List.fold_left((+), 0);

let printLayers = layers =>
  layers
  |> List.iter(initial => {
       initial
       |> Array.iter(row => {
            Array.iter(
              x =>
                switch (x) {
                | Active => print_char('#')
                | InActive => print_char('.')
                },
              row,
            );
            print_char('\n');
          });
       print_char('\n');
     });

let run = () => {
  let n = 6;
  let layers =
    Util.getLinesFromFile(path)
    |> List.rev
    |> List.map(Util.stringToCharList)
    |> List.map(List.map(of_char))
    |> List.map(Array.of_list)
    |> Array.of_list
    |> pad(n);

  let layers = times(n, layers);

  Console.log("Part 1> " ++ string_of_int(countActive(layers)));
};