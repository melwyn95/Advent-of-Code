open Angstrom;

module LuggageRule = {
  type t = (string, list((int, string)));

  let commaSpace = string(", ");
  let space = char(' ');
  let contain = string("contain");
  let bag = string("bag");
  let bags = string("bags");
  let bagsDot = string("bags.");
  let bagDot = string("bag.");
  let noOtherBags = string("no other bags.") *> return([]);
  let word = take_while1(c => c != ' ') <* space;

  let nBags =
    lift4(
      (n, c1, c2, _) => (n, c1 ++ " " ++ c2),
      word >>| int_of_string,
      word,
      word,
      bagsDot <|> bagDot <|> bags <|> bag,
    );

  let parser = {
    let color = lift2((c1, c2) => c1 ++ " " ++ c2, word, word);
    let c = bags *> space *> contain *> space;
    let containBags =
      c
      *> (
        noOtherBags <|> sep_by(commaSpace, nBags) <|> (nBags >>| (c => [c]))
      );
    lift2((c, nB) => (c, nB), color, containBags);
  };

  let parse = ruleStr =>
    switch (parse_string(~consume=All, parser, ruleStr)) {
    | Ok(t) => t
    | Error(msg) => failwith(msg)
    };
};

let path = "./bin/input_day_7";
let testPath = "./bin/input_day_7_test";

type hashTableType = Hashtbl.t(string, list((int, string)));

/* TODO: memoize this function */
/* very sub optimal code optimize later */
let rec dfs = (xs: list((int, string)), htbl: hashTableType, search: string) => {
  switch (xs) {
  | [] => false
  | [(_, x), ...tl] =>
    x == search
    || dfs(Hashtbl.find(htbl, x), htbl, search)
    || dfs(tl, htbl, search)
  };
};

/* TODO: memoize this function */
/* very sub optimal code optimize later */
let rec resolveNumber = (xs: list((int, string)), htbl: hashTableType) => {
  switch (xs) {
  | [] => 0
  | [(i, x), ...tl] =>
    i
    + i
    * resolveNumber(Hashtbl.find(htbl, x), htbl)
    + resolveNumber(tl, htbl)
  };
};

/* These are very sub-optimal solutions, Needs refactor */

let run = () => {
  print_endline("---------- Day 7 ----------");
  let parsedLuggage =
    Util.getLinesFromFile(path) |> List.map(LuggageRule.parse);
  let luggageHashTbl: hashTableType =
    parsedLuggage
    |> List.fold_left(
         (acc, (k, v)) => {
           Hashtbl.add(acc, k, v);
           acc;
         },
         Hashtbl.create(List.length(parsedLuggage)),
       );

  let resolvedLuggage =
    parsedLuggage
    |> List.map(((color, bags)) => dfs(bags, luggageHashTbl, "shiny gold"))
    |> List.filter(x => x)
    |> List.length;

  Console.log("Part 1> " ++ string_of_int(resolvedLuggage));

  let (_, shinyGold) =
    parsedLuggage
    |> List.map(((color, bags)) =>
         (color, resolveNumber(bags, luggageHashTbl))
       )
    |> List.find(((s, _)) => s == "shiny gold");

  Console.log("Part 2> " ++ string_of_int(shinyGold));
};