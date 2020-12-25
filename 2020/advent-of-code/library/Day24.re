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
};

let run = () => {
  let paths = Util.getLinesFromFile(path) |> List.map(Path.parse);
  let space = Hashtbl.create(List.length(paths));
  paths
  |> List.iter(path => {
       let (x, y) = path |> List.fold_left(Path.next, (0, 0));
       if (Hashtbl.mem(space, (x, y))) {
         let b = Hashtbl.find(space, (x, y));
         Hashtbl.replace(space, (x, y), !b);
       } else {
         Hashtbl.add(space, (x, y), true);
       };
     });
  let blackCells =
    Hashtbl.fold((_, b, count) => b ? count + 1 : count, space, 0);

  Console.log(blackCells);

  ();
};