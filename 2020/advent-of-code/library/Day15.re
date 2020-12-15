let path = "./bin/input/input_day_15";
let testPath = "./bin/input/input_day_15_test";

let turns = (s, e) => {
  let rec aux = (n, xs) => {
    switch (n) {
    | _ when n < s => xs
    | _ => aux(n - 1, List.cons(n, xs))
    };
  };
  aux(e, []);
};

/* Move to Util */
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

  initial |> List.iteri((i, x) => Hashtbl.add(lookup, x, [i + 1]));

  turns(s, e)
  |> List.fold_left(
       (p, turn) => {
         let next =
           switch (Hashtbl.find_opt(lookup, p)) {
           | Some(xs) =>
             let n =
               switch (xs) {
               | [x] => 0
               | [h, t] => h - t
               | []
               | _ => failwith("invalid case")
               };
             switch (Hashtbl.find_opt(lookup, n)) {
             | None => Hashtbl.add(lookup, n, [turn])
             | Some(xs) =>
               Hashtbl.replace(lookup, n, List.cons(turn, [List.hd(xs)]))
             };
             n;
           | None =>
             switch (Hashtbl.find_opt(lookup, 0)) {
             | None => Hashtbl.add(lookup, 0, [turn])
             | Some(xs) =>
               Hashtbl.replace(lookup, 0, List.cons(turn, [List.hd(xs)]))
             };
             0;
           };
         next;
       },
       prev,
     );
};
/* Very in-efficient algorithm, need to optimize */

let findNthTermImperative = (initial, n, lookup) => {
  let prev = ref(last(initial));
  let s = List.length(initial) + 1;
  let e = n;

  let prev_tuple =
    initial
    |> List.fold_left(
         ((i, _), x) => {
           Hashtbl.add(lookup, x, (i + 1, (-1)));
           (i + 1, (-1));
         },
         (0, 0),
       )
    |> (((a, b)) => ref((a, b)));

  for (turn in s to e) {
    let next =
      switch (prev_tuple^) {
      | (x, (-1)) => 0
      | (h, t) => h - t
      };
    switch (Hashtbl.find_opt(lookup, next)) {
    | None =>
      prev_tuple := (turn, (-1));
      Hashtbl.add(lookup, next, (turn, (-1)));
    | Some((a, b)) =>
      prev_tuple := (turn, a);
      Hashtbl.replace(lookup, next, (turn, a));
    };
    prev := next;
  };
  prev^;
};

let run = () => {
  print_endline("---------- Day 15 ----------");
  let lookup = Hashtbl.create(2020);
  let initial =
    Util.getLinesFromFile(path)
    |> List.hd
    |> String.split_on_char(',')
    |> List.map(int_of_string);

  findNthTermImperative(initial, 2020, lookup)
  |> (n => Console.log("Part 1> " ++ string_of_int(n)));

  let lookup = Hashtbl.create(30000000);
  findNthTermImperative(initial, 30000000, lookup)
  |> (n => Console.log("Part 2> " ++ string_of_int(n)));
};