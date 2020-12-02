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