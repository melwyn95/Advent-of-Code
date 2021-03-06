let path = "./bin/input/input_day_20";
let testPath = "./bin/input/input_day_20_test";

type tile = {
  id: int,
  pixels: array(array(int)),
};

let makeEmptyTile = () => {id: (-1), pixels: [|[||]|]};

let printPixels = xys =>
  xys
  |> Array.map(Array.to_list)
  |> Array.iter(r =>
       Console.log(String.concat(" ", r |> List.map(string_of_int)))
     );

let flipX = xs =>
  xs |> Array.copy |> Array.to_list |> List.rev |> Array.of_list;
let flipY = xs =>
  xs
  |> Array.copy
  |> Array.map(r => r |> Array.to_list |> List.rev |> Array.of_list);
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
  let p = Array.copy(p);
  let top = p[0] |> Util.bin2dec;
  let right = p |> Array.map(row => row[len - 1]) |> Util.bin2dec;
  let bottom =
    p[len - 1] |> Array.to_list |> List.rev |> Array.of_list |> Util.bin2dec;
  let left =
    p
    |> Array.map(row => row[0])
    |> Array.to_list
    |> List.rev
    |> Array.of_list
    |> Util.bin2dec;

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

let findMatchingEdge = (edges, pixels) => {
  let (top', right', bottom', left') = getEdges(pixels);
  let (topFX', rightFX', bottomFX', leftFX') = getEdges(flipX(pixels));
  let (topFY', rightFY', bottomFY', leftFY') = getEdges(flipY(pixels));
  let matchingEdge =
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
    |> List.filter(edge => List.mem(edge, edges))
    |> List.sort_uniq(Stdlib.compare);

  matchingEdge;
};

let fixLastPixel = (tile, top, left) => {
  let pixels = tile.pixels;
  let (top', right', bottom', left') = getEdges(pixels);
  let (topFX', rightFX', bottomFX', leftFX') = getEdges(flipX(pixels));
  let (topFY', rightFY', bottomFY', leftFY') = getEdges(flipY(pixels));
  let (topFXY', rightFXY', bottomFXY', leftFXY') =
    getEdges(flipY(flipX(pixels)));

  let edges = [
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
  ];

  let topMatchingEdges = findMatchingEdge(edges, top.pixels);
  let leftMatchingEdges = findMatchingEdge(edges, left.pixels);

  let isDone = ref(false);
  let fixedPixels = ref([|[||]|]);

  topMatchingEdges
  |> List.iter(topMatchingEdge => {
       leftMatchingEdges
       |> List.iter(leftMatchingEdge =>
            if (! isDone^) {
              let pixels =
                if ((
                      leftMatchingEdge == topFY'
                      || leftMatchingEdge == rightFY'
                      || leftMatchingEdge == bottomFY'
                      || leftMatchingEdge == leftFY'
                    )
                    && (
                      topMatchingEdge == topFY'
                      || topMatchingEdge == rightFY'
                      || topMatchingEdge == bottomFY'
                      || topMatchingEdge == leftFY'
                    )) {
                  flipY(pixels);
                } else if ((
                             leftMatchingEdge == topFX'
                             || leftMatchingEdge == rightFX'
                             || leftMatchingEdge == bottomFX'
                             || leftMatchingEdge == leftFX'
                           )
                           && (
                             topMatchingEdge == topFX'
                             || topMatchingEdge == rightFX'
                             || topMatchingEdge == bottomFX'
                             || topMatchingEdge == leftFX'
                           )) {
                  flipX(pixels);
                } else if ((
                             leftMatchingEdge == topFXY'
                             || leftMatchingEdge == rightFXY'
                             || leftMatchingEdge == bottomFXY'
                             || leftMatchingEdge == leftFXY'
                           )
                           && (
                             topMatchingEdge == topFXY'
                             || topMatchingEdge == rightFXY'
                             || topMatchingEdge == bottomFXY'
                             || topMatchingEdge == leftFXY'
                           )) {
                  flipY(flipX(pixels));
                } else {
                  pixels;
                };

              let (top', right', bottom', left') = getEdges(pixels);
              let pixels =
                if (leftMatchingEdge == bottom') {
                  rotate90(pixels);
                } else if (leftMatchingEdge == left') {
                  pixels;
                } else if (leftMatchingEdge == top') {
                  rotate90(rotate90(rotate90(pixels)));
                } else if (leftMatchingEdge == right') {
                  rotate90(rotate90(pixels));
                } else {
                  pixels;
                };
              let (top', right', bottom', left') = getEdges(pixels);
              if (leftMatchingEdge == left' && topMatchingEdge == top') {
                /* Console.log("BOOM"); */
                isDone := true;
                fixedPixels := pixels;
              };
            }
          )
     });

  {id: tile.id, pixels: fixedPixels^};
};

let fixFlipsLastRow = (tile, top, right) => {
  let pixels = tile.pixels;
  let (top', right', bottom', left') = getEdges(pixels);
  let (topFX', rightFX', bottomFX', leftFX') = getEdges(flipX(pixels));
  let (topFY', rightFY', bottomFY', leftFY') = getEdges(flipY(pixels));
  let (topFXY', rightFXY', bottomFXY', leftFXY') =
    getEdges(flipY(flipX(pixels)));

  let edges = [
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
  ];

  let topMatchingEdges = findMatchingEdge(edges, top.pixels);
  let rightMatchingEdges = findMatchingEdge(edges, right.pixels);

  let isDone = ref(false);
  let fixedPixels = ref([|[||]|]);

  topMatchingEdges
  |> List.iter(topMatchingEdge => {
       rightMatchingEdges
       |> List.iter(rightMatchingEdge =>
            if (! isDone^) {
              let pixels =
                if ((
                      rightMatchingEdge == topFY'
                      || rightMatchingEdge == rightFY'
                      || rightMatchingEdge == bottomFY'
                      || rightMatchingEdge == leftFY'
                    )
                    && (
                      topMatchingEdge == topFY'
                      || topMatchingEdge == rightFY'
                      || topMatchingEdge == bottomFY'
                      || topMatchingEdge == leftFY'
                    )) {
                  flipY(pixels);
                } else if ((
                             rightMatchingEdge == topFX'
                             || rightMatchingEdge == rightFX'
                             || rightMatchingEdge == bottomFX'
                             || rightMatchingEdge == leftFX'
                           )
                           && (
                             topMatchingEdge == topFX'
                             || topMatchingEdge == rightFX'
                             || topMatchingEdge == bottomFX'
                             || topMatchingEdge == leftFX'
                           )) {
                  flipX(pixels);
                } else if ((
                             rightMatchingEdge == topFXY'
                             || rightMatchingEdge == rightFXY'
                             || rightMatchingEdge == bottomFXY'
                             || rightMatchingEdge == leftFXY'
                           )
                           && (
                             topMatchingEdge == topFXY'
                             || topMatchingEdge == rightFXY'
                             || topMatchingEdge == bottomFXY'
                             || topMatchingEdge == leftFXY'
                           )) {
                  flipY(flipX(pixels));
                } else {
                  pixels;
                };

              let (top', right', bottom', left') = getEdges(pixels);
              let pixels =
                if (rightMatchingEdge == bottom') {
                  rotate90(rotate90(rotate90(pixels)));
                } else if (rightMatchingEdge == left') {
                  rotate90(rotate90(pixels));
                } else if (rightMatchingEdge == top') {
                  rotate90(pixels);
                } else if (rightMatchingEdge == right') {
                  pixels;
                } else {
                  pixels;
                };
              let (top', right', bottom', left') = getEdges(pixels);
              if (rightMatchingEdge == right' && topMatchingEdge == top') {
                /* Console.log("BOOM"); */
                isDone := true;
                fixedPixels := pixels;
              };
            }
          )
     });

  {id: tile.id, pixels: fixedPixels^};
};

let fixFlipsLastColumn = (tile, left, bottom) => {
  let pixels = tile.pixels;
  let (top', right', bottom', left') = getEdges(pixels);
  let (topFX', rightFX', bottomFX', leftFX') = getEdges(flipX(pixels));
  let (topFY', rightFY', bottomFY', leftFY') = getEdges(flipY(pixels));
  let (topFXY', rightFXY', bottomFXY', leftFXY') =
    getEdges(flipY(flipX(pixels)));

  let edges = [
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
  ];

  let leftMatchingEdges = findMatchingEdge(edges, left.pixels);
  let bottomMatchingEdges = findMatchingEdge(edges, bottom.pixels);

  let isDone = ref(false);
  let fixedPixels = ref([|[||]|]);

  leftMatchingEdges
  |> List.iter(leftMatchingEdge => {
       bottomMatchingEdges
       |> List.iter(bottomMatchingEdge =>
            if (! isDone^) {
              let pixels =
                if ((
                      bottomMatchingEdge == topFY'
                      || bottomMatchingEdge == rightFY'
                      || bottomMatchingEdge == bottomFY'
                      || bottomMatchingEdge == leftFY'
                    )
                    && (
                      leftMatchingEdge == topFY'
                      || leftMatchingEdge == rightFY'
                      || leftMatchingEdge == bottomFY'
                      || leftMatchingEdge == leftFY'
                    )) {
                  flipY(pixels);
                } else if ((
                             bottomMatchingEdge == topFX'
                             || bottomMatchingEdge == rightFX'
                             || bottomMatchingEdge == bottomFX'
                             || bottomMatchingEdge == leftFX'
                           )
                           && (
                             leftMatchingEdge == topFX'
                             || leftMatchingEdge == rightFX'
                             || leftMatchingEdge == bottomFX'
                             || leftMatchingEdge == leftFX'
                           )) {
                  flipX(pixels);
                } else if ((
                             bottomMatchingEdge == topFXY'
                             || bottomMatchingEdge == rightFXY'
                             || bottomMatchingEdge == bottomFXY'
                             || bottomMatchingEdge == leftFXY'
                           )
                           && (
                             leftMatchingEdge == topFXY'
                             || leftMatchingEdge == rightFXY'
                             || leftMatchingEdge == bottomFXY'
                             || leftMatchingEdge == leftFXY'
                           )) {
                  flipY(flipX(pixels));
                } else {
                  pixels;
                };

              let (top', right', bottom', left') = getEdges(pixels);
              let pixels =
                if (leftMatchingEdge == bottom') {
                  rotate90(pixels);
                } else if (leftMatchingEdge == left') {
                  pixels;
                } else if (leftMatchingEdge == top') {
                  rotate90(rotate90(rotate90(pixels)));
                } else if (leftMatchingEdge == right') {
                  rotate90(rotate90(pixels));
                } else {
                  pixels;
                };
              let (top', right', bottom', left') = getEdges(pixels);
              if (leftMatchingEdge == left' && bottomMatchingEdge == bottom') {
                /* Console.log("BOOM"); */
                isDone := true;
                fixedPixels := pixels;
              };
            }
          )
     });

  {id: tile.id, pixels: fixedPixels^};
};

let fixFlips = (tile, right, bottom) => {
  let pixels = tile.pixels;
  let (top', right', bottom', left') = getEdges(pixels);
  let (topFX', rightFX', bottomFX', leftFX') = getEdges(flipX(pixels));
  let (topFY', rightFY', bottomFY', leftFY') = getEdges(flipY(pixels));
  let (topFXY', rightFXY', bottomFXY', leftFXY') =
    getEdges(flipY(flipX(pixels)));

  let edges = [
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
  ];

  let rightMatchingEdges = findMatchingEdge(edges, right.pixels);
  let bottomMatchingEdges = findMatchingEdge(edges, bottom.pixels);

  let isDone = ref(false);
  let fixedPixels = ref([|[||]|]);

  rightMatchingEdges
  |> List.iter(rightMatchingEdge => {
       bottomMatchingEdges
       |> List.iter(bottomMatchingEdge =>
            if (! isDone^) {
              let pixels =
                if ((
                      bottomMatchingEdge == topFY'
                      || bottomMatchingEdge == rightFY'
                      || bottomMatchingEdge == bottomFY'
                      || bottomMatchingEdge == leftFY'
                    )
                    && (
                      rightMatchingEdge == topFY'
                      || rightMatchingEdge == rightFY'
                      || rightMatchingEdge == bottomFY'
                      || rightMatchingEdge == leftFY'
                    )) {
                  flipY(pixels);
                } else if ((
                             bottomMatchingEdge == topFX'
                             || bottomMatchingEdge == rightFX'
                             || bottomMatchingEdge == bottomFX'
                             || bottomMatchingEdge == leftFX'
                           )
                           && (
                             rightMatchingEdge == topFX'
                             || rightMatchingEdge == rightFX'
                             || rightMatchingEdge == bottomFX'
                             || rightMatchingEdge == leftFX'
                           )) {
                  flipX(pixels);
                } else if ((
                             bottomMatchingEdge == topFXY'
                             || bottomMatchingEdge == rightFXY'
                             || bottomMatchingEdge == bottomFXY'
                             || bottomMatchingEdge == leftFXY'
                           )
                           && (
                             rightMatchingEdge == topFXY'
                             || rightMatchingEdge == rightFXY'
                             || rightMatchingEdge == bottomFXY'
                             || rightMatchingEdge == leftFXY'
                           )) {
                  flipY(flipX(pixels));
                } else {
                  pixels;
                };

              let (top', right', bottom', left') = getEdges(pixels);
              let pixels =
                if (rightMatchingEdge == bottom') {
                  rotate90(rotate90(rotate90(pixels)));
                } else if (rightMatchingEdge == left') {
                  rotate90(rotate90(pixels));
                } else if (rightMatchingEdge == top') {
                  rotate90(pixels);
                } else if (rightMatchingEdge == right') {
                  pixels;
                } else {
                  pixels;
                };
              let (top', right', bottom', left') = getEdges(pixels);
              if (rightMatchingEdge == right' && bottomMatchingEdge == bottom') {
                /* Console.log("BOOM"); */
                isDone := true;
                fixedPixels := pixels;
              };
            }
          )
     });

  {id: tile.id, pixels: fixedPixels^};
};

let removeBorder = xss => {
  let n = Array.length(xss);
  let pixels = Array.make_matrix(n - 2, n - 2, 0);
  xss
  |> Array.iteri((i, xs) =>
       xs
       |> Array.iteri((j, _) =>
            if (i >= 1 && j >= 1 && j <= n - 2 && i <= n - 2) {
              pixels[i - 1][j - 1] = xss[i][j];
            }
          )
     );
  pixels;
};

let isSeaMonsterFound = (image, i, j) => {
  image[i][j + 18] == 1
  && image[i + 1][j] == 1
  && image[i + 1][j + 5] == 1
  && image[i + 1][j + 6] == 1
  && image[i + 1][j + 11] == 1
  && image[i + 1][j + 12] == 1
  && image[i + 1][j + 17] == 1
  && image[i + 1][j + 18] == 1
  && image[i + 1][j + 19] == 1
  && image[i + 2][j + 1] == 1
  && image[i + 2][j + 4] == 1
  && image[i + 2][j + 7] == 1
  && image[i + 2][j + 10] == 1
  && image[i + 2][j + 13] == 1
  && image[i + 2][j + 16] == 1;
};

let transformImage = (image, transform) => {
  switch (transform) {
  | 1 => image
  | 2 => rotate90(image)
  | 3 => rotate90(rotate90(image))
  | 4 => rotate90(rotate90(rotate90(image)))
  | 5 => flipX(image)
  | 6 => flipX(rotate90(image))
  | 7 => flipX(rotate90(rotate90(image)))
  | 8 => flipX(rotate90(rotate90(rotate90(image))))
  | 9 => flipY(image)
  | 10 => flipY(rotate90(image))
  | 11 => flipY(rotate90(rotate90(image)))
  | 12 => flipY(rotate90(rotate90(rotate90(image))))
  | 13 => flipY(flipX(image))
  | 14 => flipY(flipX(rotate90(image)))
  | 15 => flipY(flipX(rotate90(rotate90(image))))
  | 16 => flipY(flipX(rotate90(rotate90(rotate90(image)))))
  | _ => image
  };
};

let findSeaMonsters = image => {
  let n = Array.length(image);
  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  |> List.fold_left(
       ((b, sm), transform) => {
         b
           ? (b, sm)
           : {
             let seaMonsers = ref(0);
             let newImage = transformImage(image, transform);
             newImage
             |> Array.iteri((i, xs) =>
                  xs
                  |> Array.iteri((j, _) =>
                       if (i < n - 2 && j < n - 19) {
                         if (isSeaMonsterFound(newImage, i, j)) {
                           seaMonsers := seaMonsers^ + 1;
                         };
                       }
                     )
                );
             (seaMonsers^ > 0, seaMonsers^);
           }
       },
       (false, 0),
     );
};

let run = () => {
  print_endline("---------- Day 20 ----------");
  let tiles =
    Util.getLinesFromFile(path)
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

  let grid = Array.make_matrix(n, n, makeEmptyTile());

  grid[0][0] = List.find(tile => tile.id == topLeftCorner, tiles);
  grid[0][n - 1] = List.find(tile => tile.id == topRightCorner, tiles);
  grid[n - 1][0] = List.find(tile => tile.id == bottomLeftCorner, tiles);
  grid[n - 1][n - 1] = List.find(tile => tile.id == bottomRightCorner, tiles);

  topRow
  |> List.iteri((i, tileID) =>
       grid[0][i + 1] = List.find(tile => tile.id == tileID, tiles)
     );
  bottomRow
  |> List.iteri((i, tileID) =>
       grid[n - 1][i + 1] = List.find(tile => tile.id == tileID, tiles)
     );
  leftColoumn
  |> List.iteri((i, tileID) =>
       grid[i + 1][0] = List.find(tile => tile.id == tileID, tiles)
     );
  rightColoumn
  |> List.iteri((i, tileID) =>
       grid[i + 1][n - 1] = List.find(tile => tile.id == tileID, tiles)
     );

  internalRowsLR
  |> List.iteri((i, row) =>
       row
       |> List.iteri((j, tileID) =>
            grid[i + 1][j + 1] = List.find(tile => tile.id == tileID, tiles)
          )
     );

  grid
  |> Array.iteri((i, row) => {
       row
       |> Array.iteri((j, tile) =>
            if (j + 1 < n && i + 1 < n) {
              grid[i][j] =
                fixFlips(grid[i][j], grid[i][j + 1], grid[i + 1][j]);
            } else if (j - 1 >= 0 && i + 1 < n) {
              grid[i][j] =
                fixFlipsLastColumn(
                  grid[i][j],
                  grid[i][j - 1],
                  grid[i + 1][j],
                );
            } else if (i - 1 >= 0 && j + 1 < n) {
              grid[i][j] =
                fixFlipsLastRow(grid[i][j], grid[i - 1][j], grid[i][j + 1]);
            } else {
              grid[i][j] =
                fixLastPixel(grid[i][j], grid[i - 1][j], grid[i][j - 1]);
            }
          )
     });

  grid
  |> Array.iteri((i, row) => {
       row
       |> Array.iteri((j, tile) => {
            grid[i][j] = {id: tile.id, pixels: removeBorder(tile.pixels)}
          })
     });

  let m = Array.length(grid[0][0].pixels);

  let image = Array.make_matrix(n * m, n * m, 0);

  grid
  |> Array.iteri((i, row) => {
       row
       |> Array.iteri((j, tile) => {
            let baseI = i * m;
            let baseJ = j * m;
            grid[i][j].pixels
            |> Array.iteri((x, xs) =>
                 xs
                 |> Array.iteri((y, _) => {
                      image[baseI + x][baseJ + y] = grid[i][j].pixels[x][y]
                    })
               );
          })
     });

  let blackPixels =
    image
    |> Array.fold_left(
         (count, xs) =>
           xs |> Array.fold_left((c, x) => x == 1 ? c + 1 : c, count),
         0,
       );

  let (_, seaMonsters) = findSeaMonsters(image);
  let blackPixelsInSeaMoster = 15;

  let blackPixels = blackPixels - seaMonsters * blackPixelsInSeaMoster;

  let part1 = corners |> List.fold_left(( * ), 1);
  Console.log("Part 1> " ++ string_of_int(part1));

  Console.log("Part 2> " ++ string_of_int(blackPixels));

  ();
};