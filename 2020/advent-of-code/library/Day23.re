let testInput = "389125467";
let input = "123487596";

let wrap = (i, s, e) => {
  let n = e - s + 1;
  if (i >= s && i <= e) {
    i;
  } else if (i > e) {
    i mod n;
  } else {
    n + i;
  };
};

let toString = xs =>
  xs |> Array.to_list |> List.map(string_of_int) |> String.concat(",");

let w = wrap(_, 0, 8);

let isPickedUp = ((a, b, c), i) => i == a || i == b || i == c;

let rec destination = (pickup, c, current) =>
  if (!isPickedUp(pickup, c) && c != current && c >= 1 && c <= 9) {
    c;
  } else {
    destination(pickup, wrap(c - 1, 1, 9), current);
  };

let collect = (xs, i) => {
  let rec aux = (i, n, ys) =>
    n == 0 ? ys : aux(i + 1, n - 1, List.cons(xs[w(i + 1)], ys));

  aux(i, 5, []);
};

let insert = (collect, (a, b, c), destination) => {
  let rec aux = (xs, ys) => {
    switch (xs) {
    | [] => failwith("invalid")
    | [x, ...xs] =>
      x == destination
        ? List.concat([List.rev(ys), [x], [a, b, c], xs])
        : aux(xs, List.cons(x, ys))
    };
  };
  aux(collect, []);
};

let rec assemble = (xs, insert, index) => {
  List.length(insert) == 0
    ? xs
    : {
      xs[w(index)] = List.hd(insert);
      assemble(xs, List.tl(insert), index + 1);
    };
};

let answer = xs =>
  xs
  |> Array.fold_left(
       ((ps, ns, b), x) =>
         b
           ? (ps, List.cons(x, ns), b)
           : x == 1 ? (ps, ns, true) : (List.cons(x, ps), ns, false),
       ([], [], false),
     )
  |> (((a, b, _)) => (List.rev(a), List.rev(b)));

let move = (xs, currIndex) => {
  let current = xs[w(currIndex)];
  let pickup = (
    xs[w(currIndex + 1)],
    xs[w(currIndex + 2)],
    xs[w(currIndex + 3)],
  );
  let destination = destination(pickup, current, current);

  let collect = collect(xs, w(currIndex + 3)) |> List.rev;

  let insert = insert(collect, pickup, destination);

  (assemble(xs, insert, w(currIndex + 1)), w(currIndex + 1));
};

let rec times = (n, (xs, i)) => {
  n == 0 ? xs : times(n - 1, move(xs, i));
};

external crabCupsPar2: unit => int = "caml_crabCups";

let run = () => {
  print_endline("---------- Day 23 ----------");
  let initial =
    Util.stringToCharList(input)
    |> List.map(x => int_of_char(x) - 48)
    |> Array.of_list;
  let final = times(100, (initial, 0));
  let (ps, ns) = answer(final);
  let answer =
    List.concat([ns, ps]) |> List.map(string_of_int) |> String.concat("");
  Console.log("Part 1> " ++ answer);

  let part2 = crabCupsPar2();
  Console.log("Part 2> " ++ string_of_int(part2));
};