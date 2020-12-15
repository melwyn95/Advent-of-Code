let path = "./bin/input/input_day_15";
let testPath = "./bin/input/input_day_15_test";

type hashTableType = Hashtbl.t(int, list(int));

let turns = (s, e) => {
  let rec aux = (n, xs) => {
    switch (n) {
    | _ when n < s => xs
    | _ => aux(n - 1, List.cons(n, xs))
    };
  };
  aux(e, []);
};

let findIndex = (xs, y) => {
  let rec aux = (i, ys) => {
    switch (ys) {
    | [] => None
    | [x, ...xs'] => x == y ? Some(i) : aux(i + 1, xs')
    };
  };
  aux(0, xs);
};

let rec last = xs =>
  switch (xs) {
  | [] => failwith("empty list")
  | [x] => x
  | [_, ...ys] => last(ys)
  };

let findNthTerm = (initial, n, lookup) => {
  let prev = last(initial);
  let s = List.length(initial) + 1;
  let e = n;
  turns(s, e)
  |> List.fold_left(
       (p, turn) => {
         let next =
           switch (Hashtbl.find_opt(lookup, p)) {
           | Some(xs) =>
             let n =
               switch (xs) {
               | [] => failwith("invalid case")
               | [x] =>
                 switch (findIndex(initial, p)) {
                 | Some(i) => x - (i + 1)
                 | None => 0
                 }
               | [h, ...t] => h - List.hd(t)
               };
             switch (Hashtbl.find_opt(lookup, n)) {
             | None => Hashtbl.add(lookup, n, [turn])
             | Some(xs) => Hashtbl.replace(lookup, n, List.cons(turn, xs))
             };
             n;
           | None =>
             Hashtbl.add(lookup, 0, [turn]);
             0;
           };
         next;
       },
       prev,
     );
};
/* Very in-efficient algorithm, need to optimize */

let run = () => {
  let lookup = Hashtbl.create(2020);
  let initial =
    Util.getLinesFromFile(path)
    |> List.hd
    |> String.split_on_char(',')
    |> List.map(int_of_string);

  findNthTerm(initial, 2020, lookup)
  |> (n => Console.log("Part 1> " ++ string_of_int(n)));

  let lookup = Hashtbl.create(30000000);
  findNthTerm(initial, 30000000, lookup)
  |> (n => Console.log("Part 2> " ++ string_of_int(n)));
};