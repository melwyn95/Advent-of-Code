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