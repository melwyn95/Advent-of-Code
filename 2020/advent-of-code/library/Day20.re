let path = "./bin/input/input_day_20";
let testPath = "./bin/input/input_day_20_test";

type tile = {
  id: int,
  pixels: array(array(int)),
};

let flipX = xs => xs |> Array.to_list |> List.rev |> Array.of_list;
let flipY = xs =>
  xs |> Array.map(r => r |> Array.to_list |> List.rev |> Array.of_list);
let rotate90 = xs => {
  let n = Array.length(xs);
  let matrix = Array.make_matrix(n, n, 0);
  xs
  |> Array.iteri((i, row) =>
       row |> Array.iteri((j, c) => matrix[j][n - i - 1] = c)
     );
  matrix;
};
let getEdges = p => {
  let len = Array.length(p);
  let top = p[0] |> Util.bin2dec;
  let bottom = p[len - 1] |> Util.bin2dec;
  let left = p |> Array.map(row => row[0]) |> Util.bin2dec;
  let right = p |> Array.map(row => row[len - 1]) |> Util.bin2dec;
  (top, right, bottom, left);
};

let ofStrings = xs => {
  let toInt =
    fun
    | '#' => 1
    | '.' => 0
    | _ => failwith("invalid");

  let id =
    List.hd(xs)
    |> String.split_on_char(' ')
    |> List.tl
    |> List.hd
    |> (s => String.sub(s, 0, String.length(s) - 1))
    |> int_of_string;
  let pixels =
    xs
    |> List.tl
    |> List.map(s =>
         Util.stringToCharList(s) |> List.map(toInt) |> Array.of_list
       )
    |> Array.of_list;

  {id, pixels};
};
/* TODO: [optimization] store the edges of all the tiles to avoid edge recomputation*/
let findNeighbours = (edge, unplaced) => {
  unplaced
  |> List.fold_left(
       (xs, tile) => {
         let pixels = tile.pixels;
         let (top', right', bottom', left') = getEdges(pixels);
         let (topFX', rightFX', bottomFX', leftFX') =
           getEdges(flipX(pixels));
         let (topFY', rightFY', bottomFY', leftFY') =
           getEdges(flipY(pixels));
         let allPossibleEdges = [|
           top',
           right',
           bottom',
           left',
           topFX',
           rightFX',
           bottomFX',
           leftFX',
           topFY',
           rightFY',
           bottomFY',
           leftFY',
         |];
         allPossibleEdges
         |> Array.fold_left((b, edge') => b ? b : edge == edge', false)
           ? List.cons(tile.id, xs) : xs;
       },
       [],
     );
};

let makeLookup = (lookup, tile, allTiles) => {
  let allTiles = allTiles |> List.filter(t => t.id != tile.id);
  let pixels = tile.pixels;
  let (top', right', bottom', left') = getEdges(pixels);
  let (topFX', rightFX', bottomFX', leftFX') = getEdges(flipX(pixels));
  let (topFY', rightFY', bottomFY', leftFY') = getEdges(flipY(pixels));

  let neighbours =
    [
      top',
      right',
      bottom',
      left',
      topFX',
      rightFX',
      bottomFX',
      leftFX',
      topFY',
      rightFY',
      bottomFY',
      leftFY',
    ]
    |> List.fold_left(
         (xs, edge) => List.cons(findNeighbours(edge, allTiles), xs),
         [],
       )
    |> List.flatten
    |> List.sort_uniq((t1, t2) => Stdlib.compare(t1, t2));

  Hashtbl.add(lookup, tile.id, neighbours);
};

let findEdgeRow = (lookup, tileID, edges, corners) => {
  let rec aux = (xs, tID) => {
    let x =
      Hashtbl.find(lookup, tID)
      |> List.filter(tileID =>
           List.mem(tileID, edges) && !List.mem(tileID, xs)
         );

    List.length(x) == 0
      ? List.cons(tID, xs) : aux(List.cons(tID, xs), List.hd(x));
  };
  aux([], tileID);
};

let findInternalRow = (lookup, centers, topRow, leftNeighbour) => {
  let aux = (xs, centers, topNeighbour, leftNeighbour) => {
    let tiles =
      centers
      |> List.filter_map(center => {
           let neighbours =
             Hashtbl.find(lookup, center)
             |> List.filter(n => n == topNeighbour || n == leftNeighbour);

           List.length(neighbours) == 2 ? Some(center) : None;
         });
    let tile = List.hd(tiles);
    (List.cons(tile, xs), centers |> List.filter(c => c != tile), tile);
  };
  let (xs, centers, _) =
    topRow
    |> List.fold_left(
         ((xs, centers, leftNeighbour), topNeighbour) =>
           aux(xs, centers, topNeighbour, leftNeighbour),
         ([], centers, leftNeighbour),
       );
  (xs |> List.rev, centers);
};

let run = () => {
  print_endline("---------- Day 20 ----------");
  let tiles =
    Util.getLinesFromFile(testPath)
    |> List.fold_left(
         ((curr, acc), line) =>
           line == ""
             ? ([], List.cons(curr, acc)) : (List.cons(line, curr), acc),
         ([], []),
       )
    |> (((x, xs)) => List.cons(x, xs) |> List.map(ofStrings));

  let n = int_of_float(sqrt(float_of_int(List.length(tiles))));
  let lookup = Hashtbl.create(List.length(tiles));

  tiles |> List.iter(tile => makeLookup(lookup, tile, tiles));

  let (corners, edges, centers) =
    Hashtbl.fold(
      (tileID, neighbours, (corners, edges, centers)) =>
        switch (List.length(neighbours)) {
        | 2 => (List.cons(tileID, corners), edges, centers)
        | 3 => (corners, List.cons(tileID, edges), centers)
        | 4 => (corners, edges, List.cons(tileID, centers))
        | _ => (corners, edges, centers)
        },
      lookup,
      ([], [], []),
    );

  let topLeftCorner = List.hd(corners);
  let edgeHeads = Hashtbl.find(lookup, topLeftCorner);
  let edge1 = List.hd(edgeHeads);
  let edge2 = List.hd(List.tl(edgeHeads));

  let topRow = findEdgeRow(lookup, edge1, edges, corners) |> List.rev;
  let leftColoumn = findEdgeRow(lookup, edge2, edges, corners) |> List.rev;

  let c = corners |> List.filter(c' => c' != topLeftCorner);
  let topRightCorner =
    Util.last(topRow)
    |> Hashtbl.find(lookup)
    |> List.filter(tID => List.mem(tID, c))
    |> List.hd;
  let c = c |> List.filter(c' => c' != topRightCorner);
  let bottomLeftCorner =
    Util.last(leftColoumn)
    |> Hashtbl.find(lookup)
    |> List.filter(tID => List.mem(tID, c))
    |> List.hd;
  let bottomRightCorner =
    corners
    |> List.filter(tID =>
         tID != topLeftCorner
         && tID != topRightCorner
         && tID != bottomLeftCorner
       )
    |> List.hd;

  let edgeHeads = Hashtbl.find(lookup, bottomRightCorner);
  let edge1 = List.hd(edgeHeads);
  let edge2 = List.hd(List.tl(edgeHeads));

  let bottomRow = findEdgeRow(lookup, edge1, edges, corners);
  let rightColoumn = findEdgeRow(lookup, edge2, edges, corners);

  let (internalRowsLR, _, _) =
    leftColoumn
    |> List.fold_left(
         ((xs, centers, topRow), leftftNeighbour) => {
           let (internalRow, centers) =
             findInternalRow(lookup, centers, topRow, leftftNeighbour);
           (List.cons(internalRow, xs), centers, internalRow);
         },
         ([], centers, topRow),
       );

  let internalRowsLR = internalRowsLR |> List.rev;

  let grid = Array.make_matrix(n, n, -1);

  grid[0][0] = topLeftCorner;
  grid[0][n - 1] = topRightCorner;
  grid[n - 1][0] = bottomLeftCorner;
  grid[n - 1][n - 1] = bottomRightCorner;

  topRow |> List.iteri((i, tileID) => grid[0][i + 1] = tileID);
  bottomRow |> List.iteri((i, tileID) => grid[n - 1][i + 1] = tileID);
  leftColoumn |> List.iteri((i, tileID) => grid[i + 1][0] = tileID);
  rightColoumn |> List.iteri((i, tileID) => grid[i + 1][n - 1] = tileID);

  internalRowsLR
  |> List.iteri((i, row) =>
       row |> List.iteri((j, tileID) => grid[i + 1][j + 1] = tileID)
     );

  grid
  |> Array.iter(row => {
       row
       |> Array.iter(tiledID => print_string(string_of_int(tiledID) ++ " "));
       print_string("\n");
     });

  ();
};