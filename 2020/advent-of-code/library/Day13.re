open Angstrom;

let path = "./bin/input/input_day_13";
let testPath = "./bin/input/input_day_13_test";

let run = () => {
  print_endline("---------- Day 13 ----------");
  let lines = Util.getLinesFromFile(path) |> List.rev;
  let eta = lines |> List.hd |> int_of_string;
  let schedule =
    lines
    |> List.tl
    |> List.hd
    |> String.split_on_char(',')
    |> List.filter_map(int_of_string_opt);

  schedule
  |> List.map(n => ((eta / n + 1) * n - eta, n))
  |> List.fold_left(
       ((a, m), (b, n)) => b < a ? (b, n) : (a, m),
       (max_int, 0),
     )
  |> (
    ((a, m)) => a * m |> (n => Console.log("Part 1> " ++ string_of_int(n)))
  );
};
