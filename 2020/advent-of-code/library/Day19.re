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

  let rec isResloved = t =>
    switch (t) {
    | Or(ts) => ts |> List.fold_left((acc, t) => acc && isResloved(t), true)
    | And(ts) =>
      ts |> List.fold_left((acc, t) => acc && isResloved(t), true)
    | String(_) => true
    | Reference(_) => false
    };

  let rec resolve = (t, rules) =>
    switch (t) {
    | Or(ts) => Or(ts |> List.map(resolve(_, rules)))
    | And(ts) => And(ts |> List.map(resolve(_, rules)))
    | String(s) => String(s)
    | Reference(i) =>
      switch (rules[i]) {
      | (_, r) => r
      }
    };
  let rec resolveRule = (rule, rules) =>
    if (isResloved(rule)) {
      rule;
    } else {
      resolveRule(resolve(rule, rules), rules);
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

  let isAllStrings = ts =>
    ts
    |> List.fold_left(
         (b, t) =>
           b
           && (
             switch (t) {
             | String(_) => true
             | _ => false
             }
           ),
         true,
       );
  let isAllORs = ts =>
    ts
    |> List.fold_left(
         (b, t) =>
           b
           && (
             switch (t) {
             | Or(_) => true
             | _ => false
             }
           ),
         true,
       );

  let rec resolveANDs = t =>
    switch (t) {
    | Or(ts) => Or(ts |> List.map(resolveANDs))
    | And(ts) =>
      isAllStrings(ts)
        ? String(
            List.fold_right(
              (t, s) =>
                switch (t) {
                | String(s') => s' ++ s
                | _ => failwith("not possible")
                },
              ts,
              "",
            ),
          )
        : And(ts |> List.map(resolveANDs))
    | String(s) => String(s)
    | Reference(i) => failwith("un-resolved")
    };

  let areSubExprsStrings =
    List.fold_left(
      (b, t) =>
        b
        && (
          switch (t) {
          | Or(ts) => isAllStrings(ts)
          | _ => failwith("boom")
          }
        ),
      true,
    );

  let extractStrings =
    List.map(t =>
      switch (t) {
      | Or(ts) =>
        ts
        |> List.map(t =>
             switch (t) {
             | String(s) => s
             | _ => failwith("boom")
             }
           )
      | _ => failwith("boom")
      }
    );

  let rec resolveORs = t =>
    switch (t) {
    | Or(ts) =>
      isAllORs(ts) && areSubExprsStrings(ts)
        ? Or(
            extractStrings(ts) |> combineStrings |> List.map(s => String(s)),
          )
        : Or(ts |> List.map(resolveORs))
    | And(ts) => And(ts |> List.map(resolveORs))
    | String(s) => String(s)
    | Reference(i) => failwith("un-resolved")
    };
};

let run = () => {
  let input = Util.getLinesFromFile(path) |> List.rev;
  let (rules, input) = input |> List.partition(String.contains(_, ':'));
  let rules =
    rules |> List.map(Input.parse) |> List.map(Rule.parse) |> Array.of_list;
  rules |> Array.sort(((i, _), (j, _)) => Stdlib.compare(i, j));
  let (_, rule) = rules[0];
  let rule = Rule.resolveRule(rule, rules);

  Console.log(Rule.isResloved(rule));
  let rule = Rule.resolveANDs(rule);
  let rule = Rule.resolveANDs(rule);
  let rule = Rule.resolveORs(rule);
  let rule = Rule.resolveORs(rule);
  let rule = Rule.resolveANDs(rule);
  let rule = Rule.resolveANDs(rule);
  let rule = Rule.resolveORs(rule);
  let rule = Rule.resolveORs(rule);
  let rule = Rule.resolveANDs(rule);
  let rule = Rule.resolveANDs(rule);
  let rule = Rule.resolveORs(rule);
  let rule = Rule.resolveORs(rule);

  Console.log(Rule.toString(rule));
  /* Rule.combine(rule) |> List.iter(Console.log); */
  ();
};