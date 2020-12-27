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

let findTopNeighbour = (topEdge, tiles) => {
  tiles
  |> List.fold_left(
       (n, tile) =>
         Option.is_some(n)
           ? n
           : {
             let noFlip = tile.pixels;
             let fX = flipX(noFlip);
             let fY = flipY(noFlip);
             let fXY = flipY(flipX(noFlip));
             let (topNoFlip, rightNoFlip, bottomNoFlip, leftNoFlip) =
               getEdges(noFlip);
             let (topFx, rightFx, bottomFx, leftFx) = getEdges(fX);
             let (topFy, rightFy, bottomFy, leftFy) = getEdges(fY);
             let (topFxy, rightFxy, bottomFxy, leftFxy) = getEdges(fXY);
             if (topEdge == topNoFlip
                 || topEdge == rightNoFlip
                 || topEdge == bottomNoFlip
                 || topEdge == leftNoFlip) {
               if (topEdge == topNoFlip) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(tile.pixels)),
                 });
               } else if (topEdge == rightNoFlip) {
                 Some({id: tile.id, pixels: rotate90(tile.pixels)});
               } else if (topEdge == bottomNoFlip) {
                 Some({id: tile.id, pixels: tile.pixels});
               } else {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(tile.pixels))),
                 });
               };
             } else if (topEdge == topFx
                        || topEdge == rightFx
                        || topEdge == bottomFx
                        || topEdge == leftFx) {
               if (topEdge == topFx) {
                 Some({id: tile.id, pixels: rotate90(rotate90(fX))});
               } else if (topEdge == rightFx) {
                 Some({id: tile.id, pixels: rotate90(fX)});
               } else if (topEdge == bottomFx) {
                 Some({id: tile.id, pixels: fX});
               } else {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(fX))),
                 });
               };
             } else if (topEdge == topFy
                        || topEdge == rightFy
                        || topEdge == bottomFy
                        || topEdge == leftFy) {
               if (topEdge == topFy) {
                 Some({id: tile.id, pixels: rotate90(rotate90(fY))});
               } else if (topEdge == rightFy) {
                 Some({id: tile.id, pixels: rotate90(fY)});
               } else if (topEdge == bottomFy) {
                 Some({id: tile.id, pixels: fY});
               } else {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(fY))),
                 });
               };
             } else if (topEdge == topFxy
                        || topEdge == rightFxy
                        || topEdge == bottomFxy
                        || topEdge == leftFxy) {
               if (topEdge == topFxy) {
                 Some({id: tile.id, pixels: rotate90(rotate90(fXY))});
               } else if (topEdge == rightFxy) {
                 Some({id: tile.id, pixels: rotate90(fXY)});
               } else if (topEdge == bottomFxy) {
                 Some({id: tile.id, pixels: fXY});
               } else {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(fXY))),
                 });
               };
             } else {
               None;
             };
           },
       None,
     );
};

let findRightNeighbour = (rightEdge, tiles) => {
  tiles
  |> List.fold_left(
       (n, tile) =>
         Option.is_some(n)
           ? n
           : {
             let noFlip = tile.pixels;
             let fX = flipX(noFlip);
             let fY = flipY(noFlip);
             let fXY = flipY(flipX(noFlip));
             let (topNoFlip, rightNoFlip, bottomNoFlip, leftNoFlip) =
               getEdges(noFlip);
             let (topFx, rightFx, bottomFx, leftFx) = getEdges(fX);
             let (topFy, rightFy, bottomFy, leftFy) = getEdges(fY);
             let (topFxy, rightFxy, bottomFxy, leftFxy) = getEdges(fXY);
             if (rightEdge == topNoFlip
                 || rightEdge == rightNoFlip
                 || rightEdge == bottomNoFlip
                 || rightEdge == leftNoFlip) {
               if (rightEdge == topNoFlip) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(tile.pixels))),
                 });
               } else if (rightEdge == rightNoFlip) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(tile.pixels)),
                 });
               } else if (rightEdge == bottomNoFlip) {
                 Some({id: tile.id, pixels: rotate90(tile.pixels)});
               } else {
                 Some({id: tile.id, pixels: tile.pixels});
               };
             } else if (rightEdge == topFx
                        || rightEdge == rightFx
                        || rightEdge == bottomFx
                        || rightEdge == leftFx) {
               if (rightEdge == topFx) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(fX))),
                 });
               } else if (rightEdge == rightFx) {
                 Some({id: tile.id, pixels: rotate90(rotate90(fX))});
               } else if (rightEdge == bottomFx) {
                 Some({id: tile.id, pixels: rotate90(fX)});
               } else {
                 Some({id: tile.id, pixels: fX});
               };
             } else if (rightEdge == topFy
                        || rightEdge == rightFy
                        || rightEdge == bottomFy
                        || rightEdge == leftFy) {
               if (rightEdge == topFy) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(fY))),
                 });
               } else if (rightEdge == rightFy) {
                 Some({id: tile.id, pixels: rotate90(rotate90(fY))});
               } else if (rightEdge == bottomFy) {
                 Some({id: tile.id, pixels: rotate90(fY)});
               } else {
                 Some({id: tile.id, pixels: fY});
               };
             } else if (rightEdge == topFxy
                        || rightEdge == rightFxy
                        || rightEdge == bottomFxy
                        || rightEdge == leftFxy) {
               if (rightEdge == topFxy) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(fXY))),
                 });
               } else if (rightEdge == rightFxy) {
                 Some({id: tile.id, pixels: rotate90(rotate90(fXY))});
               } else if (rightEdge == bottomFxy) {
                 Some({id: tile.id, pixels: rotate90(fXY)});
               } else {
                 Some({id: tile.id, pixels: fXY});
               };
             } else {
               None;
             };
           },
       None,
     );
};

let findBottomNeighbour = (bottomEdge, tiles) => {
  tiles
  |> List.fold_left(
       (n, tile) =>
         Option.is_some(n)
           ? n
           : {
             let noFlip = tile.pixels;
             let fX = flipX(noFlip);
             let fY = flipY(noFlip);
             let fXY = flipY(flipX(noFlip));
             let (topNoFlip, rightNoFlip, bottomNoFlip, leftNoFlip) =
               getEdges(noFlip);
             let (topFx, rightFx, bottomFx, leftFx) = getEdges(fX);
             let (topFy, rightFy, bottomFy, leftFy) = getEdges(fY);
             let (topFxy, rightFxy, bottomFxy, leftFxy) = getEdges(fXY);
             if (bottomEdge == topNoFlip
                 || bottomEdge == rightNoFlip
                 || bottomEdge == bottomNoFlip
                 || bottomEdge == leftNoFlip) {
               if (bottomEdge == topNoFlip) {
                 Some({id: tile.id, pixels: tile.pixels});
               } else if (bottomEdge == rightNoFlip) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(tile.pixels))),
                 });
               } else if (bottomEdge == bottomNoFlip) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(tile.pixels)),
                 });
               } else {
                 Some({id: tile.id, pixels: rotate90(tile.pixels)});
               };
             } else if (bottomEdge == topFx
                        || bottomEdge == rightFx
                        || bottomEdge == bottomFx
                        || bottomEdge == leftFx) {
               if (bottomEdge == topFx) {
                 Some({id: tile.id, pixels: fX});
               } else if (bottomEdge == rightFx) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(fX))),
                 });
               } else if (bottomEdge == bottomFx) {
                 Some({id: tile.id, pixels: rotate90(rotate90(fX))});
               } else {
                 Some({id: tile.id, pixels: rotate90(fX)});
               };
             } else if (bottomEdge == topFy
                        || bottomEdge == rightFy
                        || bottomEdge == bottomFy
                        || bottomEdge == leftFy) {
               if (bottomEdge == topFy) {
                 Some({id: tile.id, pixels: fY});
               } else if (bottomEdge == rightFy) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(fY))),
                 });
               } else if (bottomEdge == bottomFy) {
                 Some({id: tile.id, pixels: rotate90(rotate90(fY))});
               } else {
                 Some({id: tile.id, pixels: rotate90(fY)});
               };
             } else if (bottomEdge == topFxy
                        || bottomEdge == rightFxy
                        || bottomEdge == bottomFxy
                        || bottomEdge == leftFxy) {
               if (bottomEdge == topFxy) {
                 Some({id: tile.id, pixels: fXY});
               } else if (bottomEdge == rightFxy) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(fXY))),
                 });
               } else if (bottomEdge == bottomFxy) {
                 Some({id: tile.id, pixels: rotate90(rotate90(fXY))});
               } else {
                 Some({id: tile.id, pixels: rotate90(fXY)});
               };
             } else {
               None;
             };
           },
       None,
     );
};

let findLeftNeighbour = (leftEdge, tiles) => {
  tiles
  |> List.fold_left(
       (n, tile) =>
         Option.is_some(n)
           ? n
           : {
             let noFlip = tile.pixels;
             let fX = flipX(noFlip);
             let fY = flipY(noFlip);
             let fXY = flipY(flipX(noFlip));
             let (topNoFlip, rightNoFlip, bottomNoFlip, leftNoFlip) =
               getEdges(noFlip);
             let (topFx, rightFx, bottomFx, leftFx) = getEdges(fX);
             let (topFy, rightFy, bottomFy, leftFy) = getEdges(fY);
             let (topFxy, rightFxy, bottomFxy, leftFxy) = getEdges(fXY);
             if (leftEdge == topNoFlip
                 || leftEdge == rightNoFlip
                 || leftEdge == bottomNoFlip
                 || leftEdge == leftNoFlip) {
               if (leftEdge == topNoFlip) {
                 Some({id: tile.id, pixels: rotate90(tile.pixels)});
               } else if (leftEdge == rightNoFlip) {
                 Some({id: tile.id, pixels: tile.pixels});
               } else if (leftEdge == bottomNoFlip) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(tile.pixels))),
                 });
               } else {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(tile.pixels)),
                 });
               };
             } else if (leftEdge == topFx
                        || leftEdge == rightFx
                        || leftEdge == bottomFx
                        || leftEdge == leftFx) {
               if (leftEdge == topFx) {
                 Some({id: tile.id, pixels: rotate90(fX)});
               } else if (leftEdge == rightFx) {
                 Some({id: tile.id, pixels: fX});
               } else if (leftEdge == bottomFx) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(fX))),
                 });
               } else {
                 Some({id: tile.id, pixels: rotate90(rotate90(fX))});
               };
             } else if (leftEdge == topFy
                        || leftEdge == rightFy
                        || leftEdge == bottomFy
                        || leftEdge == leftFy) {
               if (leftEdge == topFy) {
                 Some({id: tile.id, pixels: rotate90(fY)});
               } else if (leftEdge == rightFy) {
                 Some({id: tile.id, pixels: fY});
               } else if (leftEdge == bottomFy) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(fY))),
                 });
               } else {
                 Some({id: tile.id, pixels: rotate90(rotate90(fY))});
               };
             } else if (leftEdge == topFxy
                        || leftEdge == rightFxy
                        || leftEdge == bottomFxy
                        || leftEdge == leftFxy) {
               if (leftEdge == topFxy) {
                 Some({id: tile.id, pixels: rotate90(fXY)});
               } else if (leftEdge == rightFxy) {
                 Some({id: tile.id, pixels: fXY});
               } else if (leftEdge == bottomFxy) {
                 Some({
                   id: tile.id,
                   pixels: rotate90(rotate90(rotate90(fXY))),
                 });
               } else {
                 Some({id: tile.id, pixels: rotate90(rotate90(fXY))});
               };
             } else {
               None;
             };
           },
       None,
     );
};

let findTopRightNeighbour = (topEdge, rightEdge, tiles) => {
  tiles
  |> List.fold_left(
       (n, tile) =>
         Option.is_some(n)
           ? n
           : {
             let noFlip = tile.pixels;
             let fX = flipX(noFlip);
             let fY = flipY(noFlip);
             let fXY = flipY(flipX(noFlip));
             let (topNoFlip, rightNoFlip, bottomNoFlip, leftNoFlip) =
               getEdges(noFlip);
             let (topFx, rightFx, bottomFx, leftFx) = getEdges(fX);
             let (topFy, rightFy, bottomFy, leftFy) = getEdges(fY);
             let (topFxy, rightFxy, bottomFxy, leftFxy) = getEdges(fXY);
             if (rightEdge == leftNoFlip && topEdge == bottomNoFlip) {
               Some({id: tile.id, pixels: tile.pixels});
             } else if (rightEdge == leftFx && topEdge == bottomFx) {
               Some({id: tile.id, pixels: fX});
             } else if (rightEdge == leftFy && topEdge == bottomFy) {
               Some({id: tile.id, pixels: fY});
             } else if (rightEdge == leftFxy && topEdge == bottomFxy) {
               Some({id: tile.id, pixels: fXY});
             } else {
               None;
             };
           },
       None,
     );
};

let findTopLeftNeighbour = (topEdge, leftEdge, tiles) => {
  tiles
  |> List.fold_left(
       (n, tile) =>
         Option.is_some(n)
           ? n
           : {
             let noFlip = tile.pixels;
             let fX = flipX(noFlip);
             let fY = flipY(noFlip);
             let fXY = flipY(flipX(noFlip));
             let (topNoFlip, rightNoFlip, bottomNoFlip, leftNoFlip) =
               getEdges(noFlip);
             let (topFx, rightFx, bottomFx, leftFx) = getEdges(fX);
             let (topFy, rightFy, bottomFy, leftFy) = getEdges(fY);
             let (topFxy, rightFxy, bottomFxy, leftFxy) = getEdges(fXY);
             if (leftEdge == rightNoFlip && topEdge == bottomNoFlip) {
               Some({id: tile.id, pixels: tile.pixels});
             } else if (leftEdge == rightFx && topEdge == bottomFx) {
               Some({id: tile.id, pixels: fX});
             } else if (leftEdge == rightFy && topEdge == bottomFy) {
               Some({id: tile.id, pixels: fY});
             } else if (leftEdge == rightFxy && topEdge == bottomFxy) {
               Some({id: tile.id, pixels: fXY});
             } else {
               None;
             };
           },
       None,
     );
};

let findBottomLeftNeighbour = (bottomEdge, leftEdge, tiles) => {
  tiles
  |> List.fold_left(
       (n, tile) =>
         Option.is_some(n)
           ? n
           : {
             let noFlip = tile.pixels;
             let fX = flipX(noFlip);
             let fY = flipY(noFlip);
             let fXY = flipY(flipX(noFlip));
             let (topNoFlip, rightNoFlip, bottomNoFlip, leftNoFlip) =
               getEdges(noFlip);
             let (topFx, rightFx, bottomFx, leftFx) = getEdges(fX);
             let (topFy, rightFy, bottomFy, leftFy) = getEdges(fY);
             let (topFxy, rightFxy, bottomFxy, leftFxy) = getEdges(fXY);
             if (leftEdge == rightNoFlip && bottomEdge == topNoFlip) {
               Some({id: tile.id, pixels: tile.pixels});
             } else if (leftEdge == rightFx && bottomEdge == topFx) {
               Some({id: tile.id, pixels: fX});
             } else if (leftEdge == rightFy && bottomEdge == topFy) {
               Some({id: tile.id, pixels: fY});
             } else if (leftEdge == rightFxy && bottomEdge == topFxy) {
               Some({id: tile.id, pixels: fXY});
             } else {
               None;
             };
           },
       None,
     );
};

let findBottomRightNeighbour = (bottomEdge, rightEdge, tiles) => {
  tiles
  |> List.fold_left(
       (n, tile) =>
         Option.is_some(n)
           ? n
           : {
             let noFlip = tile.pixels;
             let fX = flipX(noFlip);
             let fY = flipY(noFlip);
             let fXY = flipY(flipX(noFlip));
             let (topNoFlip, rightNoFlip, bottomNoFlip, leftNoFlip) =
               getEdges(noFlip);
             let (topFx, rightFx, bottomFx, leftFx) = getEdges(fX);
             let (topFy, rightFy, bottomFy, leftFy) = getEdges(fY);
             let (topFxy, rightFxy, bottomFxy, leftFxy) = getEdges(fXY);
             if (rightEdge == leftNoFlip && bottomEdge == topNoFlip) {
               Some({id: tile.id, pixels: tile.pixels});
             } else if (rightEdge == leftFx && bottomEdge == topFx) {
               Some({id: tile.id, pixels: fX});
             } else if (rightEdge == leftFy && bottomEdge == topFy) {
               Some({id: tile.id, pixels: fY});
             } else if (rightEdge == leftFxy && bottomEdge == topFxy) {
               Some({id: tile.id, pixels: fXY});
             } else {
               None;
             };
           },
       None,
     );
};

let rec placeNeighbours = (grid, current, unplaced, (x, y)) =>
  if (List.length(unplaced) == 0) {
    [];
  } else {
    let (top, right, bottom, left) = getEdges(current.pixels);
    let topNeighbour = findTopNeighbour(top, unplaced);
    let rightNeighbour = findRightNeighbour(right, unplaced);
    let bottomNeighbour = findBottomNeighbour(bottom, unplaced);
    let leftNeighbour = findLeftNeighbour(left, unplaced);

    let unplaced =
      switch (topNeighbour) {
      | None => unplaced
      | Some({id, pixels}) =>
        grid[x][y - 1] = topNeighbour;
        List.filter(tile => tile.id != id, unplaced);
      };
    let unplaced =
      switch (rightNeighbour) {
      | None => unplaced
      | Some({id, pixels}) =>
        grid[x + 1][y] = rightNeighbour;
        List.filter(tile => tile.id != id, unplaced);
      };
    let unplaced =
      switch (bottomNeighbour) {
      | None => unplaced
      | Some({id, pixels}) =>
        grid[x][y + 1] = bottomNeighbour;
        List.filter(tile => tile.id != id, unplaced);
      };
    let unplaced =
      switch (leftNeighbour) {
      | None => unplaced
      | Some({id, pixels}) =>
        grid[x - 1][y] = leftNeighbour;
        List.filter(tile => tile.id != id, unplaced);
      };

    let unplaced =
      switch (topNeighbour, rightNeighbour) {
      | (Some(topNeighbour), Some(rightNeighbour)) =>
        let (_, right, _, _) = getEdges(topNeighbour.pixels);
        let (top, _, _, _) = getEdges(rightNeighbour.pixels);
        let topRightNeighbour = findTopRightNeighbour(top, right, unplaced);
        switch (topRightNeighbour) {
        | None => unplaced
        | Some({id, pixels}) =>
          grid[x - 1][y + 1] = topRightNeighbour;
          List.filter(tile => tile.id != id, unplaced);
        };
      | _ => unplaced
      };

    let unplaced =
      switch (topNeighbour, leftNeighbour) {
      | (Some(topNeighbour), Some(leftNeighbour)) =>
        let (_, _, _, left) = getEdges(topNeighbour.pixels);
        let (top, _, _, _) = getEdges(leftNeighbour.pixels);
        let topLeftNeighbour = findTopLeftNeighbour(top, left, unplaced);
        switch (topLeftNeighbour) {
        | None => unplaced
        | Some({id, pixels}) =>
          grid[x - 1][y - 1] = topLeftNeighbour;
          List.filter(tile => tile.id != id, unplaced);
        };
      | _ => unplaced
      };

    let unplaced =
      switch (bottomNeighbour, leftNeighbour) {
      | (Some(bottomNeighbour), Some(leftNeighbour)) =>
        let (_, _, _, left) = getEdges(bottomNeighbour.pixels);
        let (_, _, bottom, _) = getEdges(leftNeighbour.pixels);
        let bottomLeftNeighbour =
          findBottomLeftNeighbour(bottom, left, unplaced);
        switch (bottomLeftNeighbour) {
        | None => unplaced
        | Some({id, pixels}) =>
          grid[x + 1][y - 1] = bottomLeftNeighbour;
          List.filter(tile => tile.id != id, unplaced);
        };
      | _ => unplaced
      };

    let unplaced =
      switch (bottomNeighbour, rightNeighbour) {
      | (Some(bottomNeighbour), Some(rightNeighbour)) =>
        let (_, right, _, _) = getEdges(bottomNeighbour.pixels);
        let (_, _, bottom, _) = getEdges(rightNeighbour.pixels);
        let bottomRightNeighbour =
          findBottomRightNeighbour(bottom, right, unplaced);
        switch (bottomRightNeighbour) {
        | None => unplaced
        | Some({id, pixels}) =>
          grid[x + 1][y + 1] = bottomRightNeighbour;
          List.filter(tile => tile.id != id, unplaced);
        };
      | _ => unplaced
      };

    let unplaced =
      switch (topNeighbour) {
      | None => unplaced
      | Some({id, pixels}) =>
        placeNeighbours(grid, {id, pixels}, unplaced, (x, y - 1))
      };
    let unplaced =
      switch (rightNeighbour) {
      | None => unplaced
      | Some({id, pixels}) =>
        placeNeighbours(grid, {id, pixels}, unplaced, (x + 1, y))
      };
    let unplaced =
      switch (bottomNeighbour) {
      | None => unplaced
      | Some({id, pixels}) =>
        placeNeighbours(grid, {id, pixels}, unplaced, (x, y + 1))
      };
    let unplaced =
      switch (leftNeighbour) {
      | None => unplaced
      | Some({id, pixels}) =>
        placeNeighbours(grid, {id, pixels}, unplaced, (x - 1, y))
      };
    unplaced;
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

  let grid = Array.make_matrix(6 * n, 6 * n, None);

  grid[3 * n][3 * n] = Some(List.hd(tiles));

  let _ =
    placeNeighbours(grid, List.hd(tiles), List.tl(tiles), (3 * n, 3 * n));

  grid
  |> Array.iteri((j, row) =>
       row
       |> Array.iteri((i, t) =>
            if (Option.is_some(t)) {
              Console.log((j, i));
            }
          )
     );
  /* let grid =
       grid
       |> Array.to_list
       |> List.map(Array.to_list)
       |> List.filter_map(row => {
            let clean = row |> List.filter_map(x => x);
            List.length(clean) == 0 ? None : Some(clean |> Array.of_list);
          })
       |> Array.of_list;

     let topLeftId = grid[0][0].id;
     let topRightId = grid[0][n - 1].id;
     let bottomLeftId = grid[n - 1][0].id;
     let bottomRightId = grid[n - 1][n - 1].id;

     let part1 = topLeftId * topRightId * bottomLeftId * bottomRightId;
     Console.log("Part 1> " ++ string_of_int(part1)); */
};