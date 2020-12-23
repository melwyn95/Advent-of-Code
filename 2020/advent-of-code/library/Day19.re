open Angstrom;

let path = "./bin/input/input_day_19";
let testPath = "./bin/input/input_day_19_test";

module Input = {
  type token =
    | String(string)
    | Int(int)
    | Or;

  type t = (token, list(token));

  let integer =
    take_while1(
      fun
      | '0' .. '9' => true
      | _ => false,
    )
    >>| (s => Int(int_of_string(s)));

  let stringP =
    char('"')
    *> take_while1(c => c != '"')
    <* char('"')
    >>| (s => [String(s)]);

  let pipe = char('|') *> return(Or);

  let parser = {
    let index = integer <* string(": ");
    let tokens = stringP <|> sep_by(char(' '), integer <|> pipe);
    lift2((i, t) => (i, t), index, tokens);
  };

  let parse: string => t =
    ruleString =>
      switch (parse_string(~consume=All, parser, ruleString)) {
      | Ok(t) => t
      | Error(msg) => failwith(msg)
      };
};

module Rule = {
  type t =
    | Reference(int)
    | String(string)
    | Or(list(t))
    | And(list(t));

  let rec toString = t =>
    switch (t) {
    | Reference(n) => "Reference(" ++ string_of_int(n) ++ ")"
    | String(s) => "String(" ++ s ++ ")"
    | Or(ts) => "OR(" ++ String.concat(",", List.map(toString, ts)) ++ ")"
    | And(ts) => "AND(" ++ String.concat(",", List.map(toString, ts)) ++ ")"
    };

  let modifiedRule8 =
    Or([And([Reference(8), Reference(42)]), Reference(42)]);
  let modifiedRule11 =
    Or([
      And([Reference(31), Reference(11), Reference(42)]),
      And([Reference(31), Reference(42)]),
    ]);

  let parser = its =>
    switch (its) {
    | [Input.String(s)] => String(s)
    | _ =>
      its
      |> List.fold_left(
           ((ands, ors), it) =>
             switch (it) {
             | Input.Int(n) => (List.cons(Reference(n), ands), ors)
             | Input.Or => ([], List.cons(And(ands), ors))
             | _ => failwith("boom")
             },
           ([], []),
         )
      |> (
        ((ands, ors)) =>
          List.length(ors) > 0
            ? Or(List.cons(And(ands), ors))
            : List.length(ands) == 1 ? List.hd(ands) : And(ands)
      )
    };

  let parse = inputT =>
    switch (inputT) {
    | (Input.Int(i), its) => (i, parser(its))
    | _ => failwith("boom")
    };

  let rec validate = (rules, rule, input, index) => {
    switch (rule) {
    | Reference(n) =>
      let (_, rule) = rules |> List.find(((i, _)) => i == n);
      validate(rules, rule, input, index);
    | String(s) => [
        (
          index < String.length(input) && s == Char.escaped(input.[index]),
          index + 1,
        ),
      ]
    | And(rules') =>
      rules'
      |> List.rev
      |> List.fold_left(
           (vs, rule) =>
             vs
             |> List.map(((isValid, index)) =>
                  isValid
                    ? validate(rules, rule, input, index) : [(false, index)]
                )
             |> List.flatten,
           [(true, index)],
         )
    | Or(rules') =>
      rules'
      |> List.rev
      |> List.map(rule => validate(rules, rule, input, index))
      |> List.flatten
      |> List.filter(((isValid, _)) => isValid)
    };
  };
};

let run = () => {
  print_endline("---------- Day 19 ----------");
  let input = Util.getLinesFromFile(path) |> List.rev;
  let (rules, inputs) = input |> List.partition(String.contains(_, ':'));
  let rules = rules |> List.map(Input.parse) |> List.map(Rule.parse);
  let (_, rule) = rules |> List.find(((i, _)) => i == 0);

  let inputs = List.tl(inputs); /* first item will be empty string */
  let validRules =
    inputs
    |> List.map(input => {
         let (isValid, index) =
           Rule.validate(rules, rule, input, 0)
           |> (x => List.length(x) > 0 ? List.hd(x) : (false, (-1)));

         isValid ? index == String.length(input) : false;
       })
    |> List.filter(x => x)
    |> List.length;

  Console.log("Part 1> " ++ string_of_int(validRules));

  let input = Util.getLinesFromFile(path) |> List.rev;
  let (rules, inputs) = input |> List.partition(String.contains(_, ':'));
  let rules =
    rules
    |> List.map(Input.parse)
    |> List.map(Rule.parse)
    |> List.map(((n, rule)) =>
         switch (n) {
         | 8 => (n, Rule.modifiedRule8)
         | 11 => (n, Rule.modifiedRule11)
         | _ => (n, rule)
         }
       );
  let (_, rule) = rules |> List.find(((i, _)) => i == 0);

  let inputs = List.tl(inputs); /* first item will be empty string */
  let validRules =
    inputs
    |> List.map(input => {
         let (isValid, index) =
           Rule.validate(rules, rule, input, 0)
           |> (x => List.length(x) > 0 ? List.hd(x) : (false, (-1)));

         isValid ? index == String.length(input) : false;
       })
    |> List.filter(x => x)
    |> List.length;

  Console.log("Part 2> " ++ string_of_int(validRules));
};