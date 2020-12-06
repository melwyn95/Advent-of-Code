let path = "./bin/input_day_6";
let testPath = "./bin/input_day_6_test";

module CharSet = Set.Make(Char);

let prepareInput = xs => {
  xs
  |> List.fold_left(
       ((str, acc), x) =>
         if (x == "") {
           ("", List.cons(str, acc));
         } else {
           (str ++ x, acc);
         },
       ("", []),
     )
  |> (((str, acc)) => List.cons(str, acc));
};

let stringToSet = s => s |> Util.stringToCharList |> CharSet.of_list;

let prepareInput2 = xs => {
  xs
  |> List.fold_left(
       ((sets, listSets), x) =>
         if (x == "") {
           ([], List.cons(sets, listSets));
         } else {
           (List.cons(stringToSet(x), sets), listSets);
         },
       ([], []),
     )
  |> (((sets, listSets)) => List.cons(sets, listSets));
};

let run = () => {
  print_endline("---------- Day 6 ----------");
  let groupAnswers = Util.getLinesFromFile(path) |> prepareInput;
  let numberOfAnswers =
    groupAnswers
    |> List.map(Util.stringToCharList)
    |> List.map(List.fold_right(CharSet.add))
    |> List.map(f => f(CharSet.empty))
    |> List.map(CharSet.elements)
    |> List.map(List.length)
    |> List.fold_left((+), 0);

  Console.log("Part 1> " ++ string_of_int(numberOfAnswers));

  let numberOfAnswers2 =
    Util.getLinesFromFile(path)
    |> prepareInput2
    |> List.map(setList =>
         List.tl(setList) |> List.fold_left(CharSet.inter, List.hd(setList))
       )
    |> List.map(CharSet.elements)
    |> List.map(List.length)
    |> List.fold_left((+), 0);
  Console.log("Part 2> " ++ string_of_int(numberOfAnswers2));
};