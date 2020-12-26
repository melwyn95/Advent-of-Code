open Angstrom;

let path = "./bin/input/input_day_24";
let testPath = "./bin/input/input_day_24_test";

module Path = {
  type t =
    | East
    | SouthEast
    | SouthWest
    | West
    | NorthWest
    | NorthEast;

  let toString = ts =>
    ts
    |> List.map(
         fun
         | East => "E|"
         | SouthEast => "SE|"
         | SouthWest => "SW|"
         | West => "W|"
         | NorthWest => "NW|"
         | NorthEast => "NE|",
       );

  let east = string("e") >>| (_ => East);
  let southEast = string("se") >>| (_ => SouthEast);
  let southWest = string("sw") >>| (_ => SouthWest);
  let west = string("w") >>| (_ => West);
  let northWest = string("nw") >>| (_ => NorthWest);
  let northEast = string("ne") >>| (_ => NorthEast);
  let parser =
    many(
      east <|> southEast <|> southWest <|> west <|> northWest <|> northEast,
    );

  let parse = directions =>
    switch (parse_string(~consume=All, parser, directions)) {
    | Ok(ts) => ts
    | Error(msg) => failwith(msg)
    };

  let next = ((x, y), t) =>
    switch (t) {
    | East => (x + 1, y)
    | SouthEast => abs(y) mod 2 == 0 ? (x, y + 1) : (x + 1, y + 1)
    | SouthWest => abs(y) mod 2 == 0 ? (x - 1, y + 1) : (x, y + 1)
    | West => (x - 1, y)
    | NorthWest => abs(y) mod 2 == 0 ? (x - 1, y - 1) : (x, y - 1)
    | NorthEast => abs(y) mod 2 == 0 ? (x, y - 1) : (x + 1, y - 1)
    };

  let neighbours = p => {
    [|East, SouthEast, SouthWest, West, NorthWest, NorthEast|]
    |> Array.map(next(p));
  };
};

let blackCells = (space, p) =>
  Path.neighbours(p)
  |> Array.fold_left(
       (c, p') =>
         switch (Hashtbl.find_opt(space, p')) {
         | None => c
         | Some(b) => b ? c + 1 : c
         },
       0,
     );

let rec generateNeighbours = (xys, hash, n) =>
  if (n > 0) {
    let neighbours =
      xys
      |> List.map(Path.neighbours)
      |> Array.concat
      |> Array.fold_left(
           (ps, p) =>
             if (!Hashtbl.mem(hash, p)) {
               Hashtbl.add(hash, p, false);
               List.cons(p, ps);
             } else {
               ps;
             },
           [],
         );
    generateNeighbours(neighbours, hash, n - 1);
  };

let rec day = (space, n) =>
  if (n == 0) {
    ();
  } else {
    let toFlip =
      Hashtbl.fold(
        (p, b, xs) => {
          let blackCells = blackCells(space, p);
          b
            ? blackCells == 0 || blackCells > 2 ? List.cons(p, xs) : xs
            : blackCells == 2 ? List.cons(p, xs) : xs;
        },
        space,
        [],
      );

    toFlip
    |> List.iter(p => {
         let b = Hashtbl.find(space, p);
         Hashtbl.replace(space, p, !b);
       });
    day(space, n - 1);
  };

let run = () => {
  let paths = Util.getLinesFromFile(path) |> List.map(Path.parse);
  /* generate all neighbours */
  let space = Hashtbl.create(1000000);
  Hashtbl.add(space, (0, 0), false);
  generateNeighbours([(0, 0)], space, 200);

  paths
  |> List.iter(path => {
       let (x, y) = path |> List.fold_left(Path.next, (0, 0));
       if (Hashtbl.mem(space, (x, y))) {
         let b = Hashtbl.find(space, (x, y));
         Hashtbl.replace(space, (x, y), !b);
       };
     });
  let blackCells =
    Hashtbl.fold((_, b, count) => b ? count + 1 : count, space, 0);

  Console.log("Part 1> " ++ string_of_int(blackCells));

  day(space, 100);

  let blackCells =
    Hashtbl.fold((_, b, count) => b ? count + 1 : count, space, 0);

  Console.log("Part 2> " ++ string_of_int(blackCells));
};