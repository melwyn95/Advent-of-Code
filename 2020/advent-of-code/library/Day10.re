let path = "./bin/input_day_10";
let testPath = "./bin/input_day_10_test";

type hashTableType = Hashtbl.t(int, int);

let rec binarySearch = (s, e, x, xs) =>
  if (s <= e) {
    let midIndex = (s + e) / 2;
    let mid = xs[midIndex];
    if (mid == x) {
      1;
    } else if (x > mid) {
      binarySearch(midIndex + 1, e, x, xs);
    } else {
      binarySearch(s, midIndex - 1, x, xs);
    };
  } else {
    0;
  };

let run = () => {
  print_endline("---------- Day 10 ----------");
  let adapters =
    Util.getLinesFromFile(path)
    |> List.map(int_of_string)
    |> List.sort(Stdlib.compare);

  let length = List.length(adapters);

  let diffJoltages =
    adapters
    |> List.fold_left(
         ((p, xs), x) => (x, List.cons(x - p, xs)),
         (0, []),
       )
    |> (((_, xs)) => List.cons(3, xs));

  let (oneDiff, threeDiff) = diffJoltages |> List.partition(x => x == 1);

  let answerPart1 = List.length(oneDiff) * List.length(threeDiff);
  Console.log("Part 1> " ++ string_of_int(answerPart1));

  let adaptersArray = Array.of_list(adapters);

  let hashTable: hashTableType =
    [0, ...adapters]
    |> List.fold_left(
         (h, x) => {
           let value =
             binarySearch(0, length - 1, x + 1, adaptersArray)
             + binarySearch(0, length - 1, x + 2, adaptersArray)
             + binarySearch(0, length - 1, x + 3, adaptersArray);
           let value = value == 0 ? 1 : value;
           Hashtbl.add(h, x, value);
           h;
         },
         Hashtbl.create(length),
       );

  [0, ...adapters]
  |> List.rev
  |> List.iter(x => {
       let value =
         (
           switch (binarySearch(0, length - 1, x + 1, adaptersArray)) {
           | 0 => 0
           | _ => Hashtbl.find(hashTable, x + 1)
           }
         )
         + (
           switch (binarySearch(0, length - 1, x + 2, adaptersArray)) {
           | 0 => 0
           | _ => Hashtbl.find(hashTable, x + 2)
           }
         )
         + (
           switch (binarySearch(0, length - 1, x + 3, adaptersArray)) {
           | 0 => 0
           | _ => Hashtbl.find(hashTable, x + 3)
           }
         );
       let value = value == 0 ? 1 : value;
       Hashtbl.replace(hashTable, x, value);
     });

  let answerPart2 = Hashtbl.find(hashTable, 0);
  Console.log("Part 2> " ++ string_of_int(answerPart2));
};