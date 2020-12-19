open Angstrom;

let path = "./bin/input/input_day_13";
let testPath = "./bin/input/input_day_13_test";

let moduloInverse = (a, b) => {
  let rec gcd = (a, b, xs) =>
    switch (a, b) {
    | (a, 0) => (a, xs)
    | (a, b) => gcd(b, a mod b, List.cons(((a, 1), (b, - (a / b))), xs))
    };
  let (_, xs) = gcd(b, a, []);
  if (List.length(xs) == 0) {
    0;
  } else {
    let xs = List.tl(xs);
    let h = List.hd(xs);
    xs
    |> List.tl
    |> List.fold_left(
         (a, b) => {
           let ((g, x), (_, y)) = a;
           let ((p, r), (s, t)) = b;
           ((p, r * y), (g, x + t * y));
         },
         h,
       )
    |> ((((x, _), (_, y))) => x + y);
  };
};

let chineseRemainderTheorem = xs => {
  let as_ = xs |> List.map(((a, _)) => a);
  let ns = xs |> List.map(((_, x)) => x);
  let n = ns |> List.fold_left(( * ), 1);
  let ys = ns |> List.map(ni => n / ni);
  let zs = List.map2(moduloInverse, ys, ns);
  let yzs = List.map2(( * ), ys, zs);
  let x = List.map2(( * ), yzs, as_) |> List.fold_left((+), 0);
  x mod n;
};

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

  let part2 =
    lines
    |> List.tl
    |> List.hd
    |> String.split_on_char(',')
    |> List.mapi((i, s) =>
         switch (int_of_string_opt(s)) {
         | Some(n) => Some((n - i == n ? 0 : n - i, n))
         | None => None
         }
       )
    |> List.filter_map(x => x)
    |> chineseRemainderTheorem;

  Console.log("Part 2> " ++ string_of_int(part2));
};