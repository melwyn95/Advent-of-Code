let path = "./bin/input/input_day_9";
let preambleSize = 25;
let testPath = "./bin/input/input_day_9_test";
let testPreambleSize = 5;

/* TODO: refactor this */
let rec find = (s, e, x, xs) =>
  if (s <= e) {
    if (xs[s] == x) {
      true;
    } else {
      find(s + 1, e, x, xs);
    };
  } else {
    false;
  };

/* TODO: refactor this */
let isNumberCorrupted = (numbers, start, length, x) => {
  let rec aux = c =>
    if (c == start + length - 1) {
      true;
    } else if (find(start, start + length - 1, abs(numbers[c] - x), numbers)) {
      false;
    } else {
      aux(c + 1);
    };
  aux(start);
};

/* TODO: refactor this */
let rec findCorruptedNumber = (numbers, i, preambleSize) => {
  let current = numbers[i];
  if (isNumberCorrupted(numbers, i - preambleSize, preambleSize, current)) {
    current;
  } else {
    findCorruptedNumber(numbers, i + 1, preambleSize);
  };
};

let rec findSumInvalid = (numbers, start, invalidNumber) => {
  let rec aux = (sum, i, xs) =>
    switch (sum) {
    | 0 => (List.length(xs) >= 2, xs)
    | n when n < 0 => (false, xs)
    | _ => aux(sum - numbers[i], i + 1, List.cons(numbers[i], xs))
    };

  let (isCodeBroken, xs) = aux(invalidNumber, start, []);
  if (isCodeBroken) {
    xs;
  } else {
    findSumInvalid(numbers, start + 1, invalidNumber);
  };
};

let run = () => {
  print_endline("---------- Day 9 ----------");
  let numbers =
    Util.getLinesFromFile(path)
    |> List.rev
    |> List.map(int_of_string)
    |> Array.of_list;

  let invalidNumber =
    findCorruptedNumber(numbers, preambleSize, preambleSize);
  Console.log("Part 1> " ++ string_of_int(invalidNumber));

  let encryptionWeakness =
    findSumInvalid(numbers, 0, invalidNumber)
    |> List.fold_left(
         ((mi, ma), x) =>
           if (x > ma) {
             (mi, x);
           } else if (x < mi) {
             (x, ma);
           } else {
             (mi, ma);
           },
         (max_int, - max_int),
       )
    |> (((a, b)) => a + b);
  Console.log("Part 2> " ++ string_of_int(encryptionWeakness));
};