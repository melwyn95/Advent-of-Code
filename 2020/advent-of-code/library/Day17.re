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
let nextState = (t, activeNeighbours) =>
  switch (t) {
  | Active =>
    activeNeighbours == 2 || activeNeighbours == 3 ? Active : InActive
  | InActive => activeNeighbours == 3 ? Active : InActive
  };

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
  emptyLayers
  |> List.cons(mainLayer)
  |> List.append(emptyLayers)
  |> Array.of_list;
};

let pad2 = (n, cubes) => {
  let len = cubes |> Array.length;
  let empties = Array.make(n, InActive);
  let emptyRows = Array.make(2 * n + len, InActive);

  let emptyMatrix = Array.make_matrix(2 * n + len, 2 * n + len, InActive);
  let inputRows =
    Array.concat([
      Array.make(n, emptyRows),
      cubes |> Array.map(row => Array.concat([empties, row, empties])),
      Array.make(n, emptyRows),
    ]);

  let matrix = Array.make_matrix(2 * n + 1, 2 * n + 1, emptyMatrix);
  matrix[n][n] = inputRows;
  matrix;
};

let getImmediateNeighbours = (i, j) => [
  (i - 1, j - 1),
  (i - 1, j),
  (i - 1, j + 1),
  (i, j - 1),
  (i, j),
  (i, j + 1),
  (i + 1, j - 1),
  (i + 1, j),
  (i + 1, j + 1),
];

let neighbours2 = (w, z, y, x) =>
  getImmediateNeighbours(w, z)
  |> List.map(((w', z')) =>
       getImmediateNeighbours(y, x)
       |> List.filter_map(((y', x')) =>
            x != x' || y != y' || z != z' || w != w'
              ? Some((w', z', y', x')) : None
          )
     );

let getNeighbours = (z, y, x) =>
  List.map(
    z' =>
      getImmediateNeighbours(y, x)
      |> List.filter_map(((y', x')) =>
           x != x' || y != y' || z != z' ? Some((z', y', x')) : None
         ),
    [z - 1, z, z + 1],
  );

let getActiveNeighboursFromLayer = (indices, layers) => {
  let zn = Array.length(layers);
  let yn = Array.length(layers[0]);
  let xn = Array.length(layers[0][0]);
  indices
  |> List.filter_map(((z, y, x)) =>
       if (z < 0 || y < 0 || x < 0 || z == zn || y == yn || x == xn) {
         None;
       } else {
         switch (layers[z][y][x]) {
         | Active => Some(Active)
         | InActive => None
         };
       }
     );
};

let getActiveNeighbour2 = (indices, layer) => {
  let wn = Array.length(layer);
  let zn = Array.length(layer[0]);
  let yn = Array.length(layer[0][0]);
  let xn = Array.length(layer[0][0][0]);
  indices
  |> List.filter_map(((w, z, y, x)) =>
       if (w < 0
           || z < 0
           || y < 0
           || x < 0
           || w == wn
           || z == zn
           || y == yn
           || x == xn) {
         None;
       } else {
         switch (layer[w][z][y][x]) {
         | Active => Some(Active)
         | InActive => None
         };
       }
     );
};

let processLayer2 = layers => {
  layers
  |> Array.mapi((w, ws) => {
       ws
       |> Array.mapi((z, zs) => {
            zs
            |> Array.mapi((y, ys) => {
                 ys
                 |> Array.mapi((x, x') => {
                      neighbours2(w, z, y, x)
                      |> List.fold_left(
                           (s, n) =>
                             s
                             + (getActiveNeighbour2(n, layers) |> List.length),
                           0,
                         )
                      |> nextState(x')
                    })
               })
          })
     });
};

let countActive2 = layers => {
  layers
  |> Array.fold_left(
       (s, x) =>
         x
         |> Array.fold_left(
              (s1, y) =>
                y
                |> Array.fold_left(
                     (s2, z) =>
                       z
                       |> Array.fold_left(
                            (s3, t) =>
                              switch (t) {
                              | Active => s3 + 1
                              | InActive => s3
                              },
                            s2,
                          ),
                     s1,
                   ),
              s,
            ),
       0,
     );
};

let processLayer = layers => {
  layers
  |> Array.mapi((z, zs) =>
       zs
       |> Array.mapi((y, ys) =>
            ys
            |> Array.mapi((x, x') => {
                 getNeighbours(z, y, x)
                 |> List.fold_left(
                      (s, n) =>
                        s
                        + (
                          getActiveNeighboursFromLayer(n, layers)
                          |> List.length
                        ),
                      0,
                    )
                 |> nextState(x')
               })
          )
     );
};

let rec times = (n, layers, process) => {
  switch (n) {
  | 0 => layers
  | _ => times(n - 1, process(layers), process)
  };
};

let countActive = layers =>
  layers
  |> Array.fold_left(
       (s, layer) =>
         s
         + (
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
         ),
       0,
     );

let run = () => {
  print_endline("---------- Day 17 ----------");
  let input =
    Util.getLinesFromFile(path)
    |> List.rev
    |> List.map(Util.stringToCharList)
    |> List.map(List.map(of_char))
    |> List.map(Array.of_list)
    |> Array.of_list;

  let n = 6;
  let layers = input |> pad(n);

  let layers = times(n, layers, processLayer);
  let count = countActive(layers);

  Console.log("Part 1> " ++ string_of_int(count));

  let layers = input |> pad2(n);
  let layers = times(n, layers, processLayer2);
  let count = countActive2(layers);

  Console.log("Part 2> " ++ string_of_int(count));
};