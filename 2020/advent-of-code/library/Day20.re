let path = "./bin/input/input_day_20";
let testPath = "./bin/input/input_day_20_test";

type pixels = {
  p: array(array(int)),
  top: int,
  right: int,
  bottom: int,
  left: int,
};

type tile = {
  id: int,
  noFlip: pixels,
  flipX: pixels,
  flipY: pixels,
  flipXY: pixels,
};

let ofStrings = xs => {
  let toInt =
    fun
    | '#' => 1
    | '.' => 0
    | _ => failwith("invalid");
  let getEdges = p => {
    let len = Array.length(p);
    let top = p[0] |> Util.bin2dec;
    let bottom = p[len - 1] |> Util.bin2dec;
    let left = p |> Array.map(row => row[0]) |> Util.bin2dec;
    let right = p |> Array.map(row => row[len - 1]) |> Util.bin2dec;
    (top, right, bottom, left);
  };

  let id =
    List.hd(xs)
    |> String.split_on_char(' ')
    |> List.tl
    |> List.hd
    |> (s => String.sub(s, 0, String.length(s) - 1))
    |> int_of_string;
  let p =
    xs
    |> List.tl
    |> List.map(s =>
         Util.stringToCharList(s) |> List.map(toInt) |> Array.of_list
       )
    |> Array.of_list;
  let (top, right, bottom, left) = getEdges(p);
  let noFlip = {p, top, right, bottom, left};
  let p = noFlip.p |> Array.to_list |> List.rev |> Array.of_list;
  let (top, right, bottom, left) = getEdges(p);
  let flipX = {p, top, right, bottom, left};
  let p =
    noFlip.p |> Array.map(r => r |> Array.to_list |> List.rev |> Array.of_list);
  let (top, right, bottom, left) = getEdges(p);
  let flipY = {p, top, right, bottom, left};
  let p = noFlip.p |> Array.to_list |> List.rev |> Array.of_list;
  let p = p |> Array.map(r => r |> Array.to_list |> List.rev |> Array.of_list);
  let (top, right, bottom, left) = getEdges(p);
  let flipXY = {p, top, right, bottom, left};

  {id, noFlip, flipX, flipY, flipXY};
};

type neighbours = {
  id: int,
  top: option(int),
  right: option(int),
  bottom: option(int),
  left: option(int),
};

let make = (~id, ~top, ~right, ~bottom, ~left, ()) => {
  id,
  top,
  right,
  bottom,
  left,
};

let checkTopEdge = (edge: pixels, tile) => {
  let noFlip = tile.noFlip;
  let flipX = tile.flipX;
  let flipY = tile.flipY;
  let flipXY = tile.flipXY;

  [noFlip, flipX, flipY, flipXY]
  |> List.fold_left(
       (acc, f: pixels) => {
         !acc
           ? edge.top == f.right
             || edge.top == f.left
             || edge.top == f.top
             || edge.top == f.bottom
           : acc
       },
       false,
     );
};

let checkRightEdge = (edge: pixels, tile: tile) => {
  let noFlip = tile.noFlip;
  let flipX = tile.flipX;
  let flipY = tile.flipY;
  let flipXY = tile.flipXY;

  [noFlip, flipX, flipY, flipXY]
  |> List.fold_left(
       (acc, f: pixels) => {
         !acc
           ? edge.right == f.right
             || edge.right == f.left
             || edge.right == f.top
             || edge.right == f.bottom
           : acc
       },
       false,
     );
};

let checkBottomEdge = (edge: pixels, tile) => {
  let noFlip = tile.noFlip;
  let flipX = tile.flipX;
  let flipY = tile.flipY;
  let flipXY = tile.flipXY;

  [noFlip, flipX, flipY, flipXY]
  |> List.fold_left(
       (acc, f: pixels) => {
         !acc
           ? edge.bottom == f.right
             || edge.bottom == f.left
             || edge.bottom == f.top
             || edge.bottom == f.bottom
           : acc
       },
       false,
     );
};

let checkLeftEdge = (edge: pixels, tile) => {
  let noFlip = tile.noFlip;
  let flipX = tile.flipX;
  let flipY = tile.flipY;
  let flipXY = tile.flipXY;

  [noFlip, flipX, flipY, flipXY]
  |> List.fold_left(
       (acc, f: pixels) => {
         !acc
           ? edge.left == f.right
             || edge.left == f.left
             || edge.left == f.top
             || edge.left == f.bottom
           : acc
       },
       false,
     );
};

let findNeigbbours: (list(tile), tile) => neighbours =
  (tiles, tile) => {
    let id = tile.id;
    let top =
      tiles
      |> List.find_opt((tile': tile) =>
           id != tile'.id && checkTopEdge(tile.noFlip, tile')
         )
      |> Option.map((t: tile) => t.id);
    let right =
      tiles
      |> List.find_opt((tile': tile) =>
           id != tile'.id && checkRightEdge(tile.noFlip, tile')
         )
      |> Option.map((t: tile) => t.id);
    let bottom =
      tiles
      |> List.find_opt((tile': tile) =>
           id != tile'.id && checkBottomEdge(tile.noFlip, tile')
         )
      |> Option.map((t: tile) => t.id);
    let left =
      tiles
      |> List.find_opt((tile': tile) =>
           id != tile'.id && checkLeftEdge(tile.noFlip, tile')
         )
      |> Option.map((t: tile) => t.id);
    make(~id, ~top, ~right, ~bottom, ~left, ());
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

  let neighbours = tiles |> List.map(findNeigbbours(tiles));

  neighbours
  |> List.filter((n: neighbours) =>
       [n.top, n.right, n.bottom, n.left]
       |> List.filter_map(x => x)
       |> List.length == 2
     )
  |> List.map(n => n.id)
  |> List.fold_left(( * ), 1)
  |> (p => Console.log("Part 1> " ++ string_of_int(p)));
};