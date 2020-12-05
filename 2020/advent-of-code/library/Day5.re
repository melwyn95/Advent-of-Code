let path = "./bin/input_day_5";
let testPath = "./bin/input_day_5_test";

let rec binaryRange = (index, sr, er, sc, ec, str) =>
  if (index < String.length(str)) {
    switch (str.[index]) {
    | 'F' => binaryRange(index + 1, sr, (sr + er) / 2, sc, ec, str)
    | 'B' => binaryRange(index + 1, (sr + er) / 2, er, sc, ec, str)
    | 'L' => binaryRange(index + 1, sr, er, sc, (sc + ec) / 2, str)
    | 'R' => binaryRange(index + 1, sr, er, (sc + ec) / 2, ec, str)
    | _ => ((-1), (-1), (-1), (-1))
    };
  } else {
    (sr, er, sc, ec);
  };

let getSeatID = ((_, a, _, b)) => a * 8 + b;

let rec binarySearch = (s, e, x, xs) =>
  if (s <= e) {
    let midIndex = (s + e) / 2;
    let mid = xs[midIndex];
    if (mid == x) {
      true;
    } else if (x > mid) {
      binarySearch(midIndex + 1, e, x, xs);
    } else {
      binarySearch(s, midIndex - 1, x, xs);
    };
  } else {
    false;
  };

let rec upto = (s, e, xs, len) =>
  if (s < e) {
    if (binarySearch(0, len - 1, s, xs)) {
      upto(s + 1, e, xs, len);
    } else {
      s;
    };
  } else {
    (-1);
  };

let run = () => {
  print_endline("---------- Day 5 ----------");
  Util.getLinesFromFile(path)
  |> List.map(binaryRange(0, 0, 127, 0, 7))
  |> List.map(getSeatID)
  |> List.fold_left(max, min_int)
  |> (n => Console.log("Part 1> " ++ string_of_int(n)));

  let sortedSeatIDs =
    Util.getLinesFromFile(path)
    |> List.map(binaryRange(0, 0, 127, 0, 7))
    |> List.map(getSeatID)
    |> List.sort(Stdlib.compare)
    |> Array.of_list;

  let length = Array.length(sortedSeatIDs);

  let (min, max) = (sortedSeatIDs[0], sortedSeatIDs[length - 1]);

  upto(min, max, sortedSeatIDs, length)
  |> (n => Console.log("Part 2> " ++ string_of_int(n)));
};