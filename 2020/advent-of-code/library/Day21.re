open Angstrom;

let path = "./bin/input/input_day_21";
let testPath = "./bin/input/input_day_21_test";

module Food = {
  type parts =
    | Ingredient(string)
    | Allergens(list(string));

  type t = {
    ingredients: list(string),
    allergens: list(string),
  };

  let stringsParser = take_while1(c => c != ' ' && c != ')' && c != ',');
  let allergensParser =
    string("(contains ")
    *> (sep_by(string(", "), stringsParser) >>| (s => Allergens(s)))
    <* char(')');

  let parser =
    sep_by(
      char(' '),
      allergensParser <|> (stringsParser >>| (s => Ingredient(s))),
    );

  let combine = parts =>
    parts
    |> List.fold_left(
         ((ings, alrs), part) =>
           switch (part) {
           | Ingredient(s) => (List.cons(s, ings), alrs)
           | Allergens(xs) => (ings, xs)
           },
         ([], []),
       )
    |> (((ingredients, allergens)) => {ingredients, allergens});

  let parse = foodString =>
    switch (parse_string(~consume=All, parser, foodString)) {
    | Ok(t) => t |> combine
    | Error(msg) => failwith(msg)
    };
};

let resolve = xs => {
  let rec aux = (unresolved, allergens, ingredients) =>
    if (List.length(unresolved) == 0) {
      (allergens, ingredients);
    } else {
      let resolved =
        unresolved
        |> List.filter(((_, ingredients')) =>
             List.length(ingredients') == 1
           );
      let (allergens, ingredients) =
        resolved
        |> List.fold_left(
             ((allergens, ingredients'), (allergen, ingredients)) =>
               (
                 List.cons(allergen, allergens),
                 List.concat([ingredients, ingredients']),
               ),
             (allergens, ingredients),
           );
      let unresolved =
        unresolved
        |> List.filter(((_, ingredients')) =>
             List.length(ingredients') > 1
           )
        |> List.map(((allergen, ingredients')) =>
             (
               allergen,
               ingredients' |> List.filter(ing => !List.mem(ing, ingredients)),
             )
           );
      aux(unresolved, allergens, ingredients);
    };
  aux(xs, [], []);
};

let run = () => {
  print_endline("---------- Day 21 ----------");
  let lookup = Hashtbl.create(50);
  let foods = Util.getLinesFromFile(path) |> List.map(Food.parse);

  foods
  |> List.iter(
       Food.(
         ({ingredients, allergens}) =>
           allergens
           |> List.iter(alr =>
                if (Hashtbl.mem(lookup, alr)) {
                  let ingredients' = Hashtbl.find(lookup, alr);
                  Hashtbl.replace(
                    lookup,
                    alr,
                    List.concat([ingredients, ingredients']),
                  );
                } else {
                  Hashtbl.add(lookup, alr, ingredients);
                }
              )
       ),
     );

  let unresolved =
    Hashtbl.fold(
      (alr, ingredients, xs) => {
        let uniq = List.sort_uniq(Stdlib.compare, ingredients);
        let uniq =
          uniq
          |> List.map(ing =>
               (
                 List.filter(ing' => ing == ing', ingredients) |> List.length,
                 ing,
               )
             );
        let uniq' =
          uniq
          |> List.sort(((a, _), (b, _)) => Stdlib.compare(a, b))
          |> List.rev;
        let (m, _) = List.hd(uniq');
        let uniq' =
          uniq'
          |> List.filter(((c, _)) => c == m)
          |> List.map(((_, t)) => t);

        List.cons((alr, uniq'), xs);
      },
      lookup,
      [],
    );

  let (allergens, ingredients'') = resolve(unresolved);

  let part1 =
    foods
    |> List.fold_left(
         Food.(
           (count, {ingredients, allergens}) => {
             ingredients
             |> List.fold_left(
                  (count', ing) =>
                    !List.mem(ing, ingredients'') ? count' + 1 : count',
                  count,
                );
           }
         ),
         0,
       );

  Console.log("Part 1> " ++ string_of_int(part1));

  let part2 =
    List.map2((a, i) => (a, i), allergens, ingredients'')
    |> List.sort(((a, _), (b, _)) => Stdlib.compare(a, b))
    |> List.map(((_, i)) => i)
    |> String.concat(",");

  Console.log("Part 2> " ++ part2);
};