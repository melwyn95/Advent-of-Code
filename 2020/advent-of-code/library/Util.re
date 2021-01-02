let hello = () => "hello";

let getLinesFromFile = path => {
  let ic = open_in(path);

  let rec aux = (ic, lines) =>
    try(input_line(ic) |> (x => List.cons(x, lines) |> aux(ic))) {
    | End_of_file =>
      close_in(ic);
      lines;
    };

  aux(ic, []);
};

let stringToCharList = s => {
  let chars = ref([]);
  String.iter(c => chars := List.concat([chars^, [c]]), s);
  chars^;
};

/* Logical XOR */
let (<<>>) = (x, y) =>
  switch (x, y) {
  | (true, true) => false
  | (true, false) => true
  | (false, true) => true
  | (false, false) => false
  };

let zipListOfLists = (xs, ys) => {
  let rec aux = (x, y, acc) =>
    switch (x, y) {
    | ([], [])
    | (_, [])
    | ([], _) => acc
    | ([xhd, ...xtl], [yhd, ...ytl]) =>
      aux(xtl, ytl, List.cons(List.concat([xhd, yhd]), acc))
    };

  aux(xs, ys, []);
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

let stringContains = (s1, s2) => {
  let lenS2 = String.length(s2);
  let lenS1 = String.length(s1);
  if (lenS2 <= lenS1) {
    let rec aux = (i, j) =>
      if (j == lenS2) {
        true;
      } else {
        s1.[i] == s2.[j] ? aux(i + 1, j + 1) : false;
      };
    let rec traverseS1 = i =>
      if (i == lenS1 - lenS2) {
        false;
      } else {
        aux(i, 0) ? true : traverseS1(i + 1);
      };
    traverseS1(0);
  } else {
    false;
  };
};

let combineStrings = xxs => {
  let xs = List.hd(xxs);
  if (List.length(xxs) > 1) {
    let xxs' = List.tl(xxs);
    xs
    |> List.map(x =>
         xxs'
         |> List.map(xs' => xs' |> List.map(x' => x ++ x'))
         |> List.flatten
       )
    |> List.flatten
    |> List.sort_uniq(Stdlib.compare);
  } else {
    xs;
  };
};

let bin2dec = bs => {
  let len = (bs |> Array.length) - 1;
  bs
  |> Array.mapi((i, b) => b * int_of_float(2.0 ** float_of_int(len - i)))
  |> Array.fold_left((+), 0);
};

let dec2bin = n => {
  let bin = Array.make(36, 0);
  let rec aux = (index, remaining) =>
    if (remaining == 0) {
      bin;
    } else {
      bin[index] = remaining mod 2;
      aux(index - 1, remaining / 2);
    };
  aux(35, n);
};

let keepDups = xs => {
  let rec aux = (xs, ys) => {
    switch (xs) {
    | [] => ys
    | [x, ...xs'] =>
      List.mem(x, xs') && !List.mem(x, ys)
        ? aux(xs', List.cons(x, ys)) : aux(xs', ys)
    };
  };
  aux(xs, []);
};

let rec last = xs =>
  switch (xs) {
  | [] => failwith("empty list")
  | [x] => x
  | [x, ...xs] => last(xs)
  };

let removeDups = xs => {
  let hashtb = Hashtbl.create(List.length(xs));
  xs
  |> List.iter(x =>
       if (Hashtbl.mem(hashtb, x)) {
         let count = Hashtbl.find(hashtb, x);
         Hashtbl.replace(hashtb, x, count + 1);
       } else {
         Hashtbl.add(hashtb, x, 1);
       }
     );
  xs |> List.filter(x => Hashtbl.find(hashtb, x) == 1);
};