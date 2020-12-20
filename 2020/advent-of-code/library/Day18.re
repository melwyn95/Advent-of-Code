open Angstrom;

let path = "./bin/input/input_day_18";
let testPath = "./bin/input/input_day_18_test";

module Expression = {
  type t =
    | Int(int)
    | Add
    | Mul
    | SubExpr(list(t));

  let rec toString = t =>
    switch (t) {
    | Int(n) => string_of_int(n)
    | Add => "+"
    | Mul => "*"
    | SubExpr(es) =>
      "(" ++ (List.map(toString, es) |> List.fold_left((++), "")) ++ ")"
    };

  let integer =
    take_while1(
      fun
      | '0' .. '9' => true
      | _ => false,
    )
    >>| (s => Int(int_of_string(s)));

  let add = string(" + ") *> return(Add);
  let mul = string(" * ") *> return(Mul);

  let parens = p =>
    char('(') *> p <* char(')') >>| (ts => SubExpr(ts |> List.rev));

  let chainl1 = (e, op) => {
    let rec go = acc =>
      lift2((e1, e2) => [e2, e1, ...acc], op, e) >>= go <|> return(acc);
    e >>| (e => [e]) >>= go;
  };

  let parser =
    fix(parser => {
      let term = parens(parser) <|> integer;
      chainl1(term, add <|> mul);
    });

  let parse = e =>
    switch (parse_string(~consume=All, parser, e)) {
    | Ok(ts) => ts |> List.rev
    | Error(msg) => failwith(msg)
    };

  let print = ts => {
    let rec aux = (ts, s) =>
      switch (ts) {
      | [] => s
      | [e, ...es] => aux(es, s ++ toString(e))
      };
    aux(ts, "") |> Console.log;
  };

  let eval = ts => {
    let rec first = ts =>
      switch (List.hd(ts)) {
      | Int(n) => aux(List.tl(ts), n)
      | SubExpr(e) => aux(List.tl(ts), first(e))
      | _ => failwith("boom!!!")
      }
    and aux = (es, acc) =>
      switch (es) {
      | [Add, Int(n)] => acc + n
      | [Mul, Int(n)] => acc * n
      | [Add, SubExpr(e)] => acc + first(e)
      | [Mul, SubExpr(e)] => acc * first(e)
      | [Add, Int(n), ...es'] => aux(es', acc + n)
      | [Mul, Int(n), ...es'] => aux(es', acc * n)
      | [Add, SubExpr(e), ...es'] => aux(es', acc + first(e))
      | [Mul, SubExpr(e), ...es'] => aux(es', acc * first(e))
      | _ => failwith("double boom!!!")
      };

    first(ts);
  };

  let isResolved = ts =>
    ts
    |> List.fold_left(
         (acc, t) =>
           switch (t) {
           | SubExpr(_) => false
           | Add => false
           | _ => acc
           },
         true,
       );
  let rec evalResolved = ts =>
    switch (ts) {
    | [Int(n)] => n
    | [Int(a), Mul, Int(b)] => a * b
    | [Int(a), Mul, Int(b), ...ts] => evalResolved([Int(a * b), ...ts])
    | _ => failwith("failed evalResolved")
    };
  let rec resolveExpr = ts =>
    switch (ts) {
    | [Int(n)] => [Int(n)]
    | [SubExpr(e)] => [Int(eval2(e))]
    | [Int(a), Add, Int(b)] => [Int(a + b)]
    | [Int(a), Add, SubExpr(e)] => [Int(a + eval2(e))]
    | [SubExpr(e), Add, Int(b)] => [Int(eval2(e) + b)]
    | [SubExpr(e1), Add, SubExpr(e2)] => [Int(eval2(e1) + eval2(e2))]
    | [e1, Mul, e2] => [Int(eval2([e1])), Mul, Int(eval2([e2]))]
    | [Int(a), Add, Int(b), e, ...ts] => [
        Int(a + b),
        e,
        ...resolveExpr(ts),
      ]
    | [Int(a), Add, SubExpr(e), e1, ...ts] => [
        Int(a + eval2(e)),
        e1,
        ...resolveExpr(ts),
      ]
    | [SubExpr(e), Add, Int(b), e1, ...ts] => [
        Int(eval2(e) + b),
        e1,
        ...resolveExpr(ts),
      ]
    | [SubExpr(e1), Add, SubExpr(e2), e3, ...ts] => [
        Int(eval2(e1) + eval2(e2)),
        e3,
        ...resolveExpr(ts),
      ]
    | [e1, Mul, e2, e3, ...ts] => [
        Int(eval2([e1])),
        Mul,
        ...resolveExpr([e2, e3, ...ts]),
      ]
    | _ =>
      Console.log(print(ts));
      failwith("failed resolveExpr");
    }
  and eval2 = ts =>
    isResolved(ts) ? evalResolved(ts) : eval2(resolveExpr(ts));
};

let parseEval = e => e |> Expression.parse |> Expression.eval;

let testExpressions1 = () => {
  parseEval("1 + 2 * 3 + 4 * 5 + 6")
  |> (v => Console.log((v, v == 71 ? "pass" : "fail")));
  parseEval("1 + (2 * 3) + (4 * (5 + 6))")
  |> (v => Console.log((v, v == 51 ? "pass" : "fail")));
  parseEval("2 * 3 + (4 * 5)")
  |> (v => Console.log((v, v == 26 ? "pass" : "fail")));
  parseEval("5 + (8 * 3 + 9 + 3 * 4 * 3)")
  |> (v => Console.log((v, v == 437 ? "pass" : "fail")));
  parseEval("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
  |> (v => Console.log((v, v == 12240 ? "pass" : "fail")));
  parseEval("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
  |> (v => Console.log((v, v == 13632 ? "pass" : "fail")));
};

let testExpressions2 = () => {
  Expression.parse("1 + 2 * 3 + 4 * 5 + 6")
  |> Expression.eval2
  |> (v => Console.log((v, v == 231 ? "pass" : "fail")));
  Expression.parse("1 + (2 * 3) + (4 * (5 + 6))")
  |> Expression.eval2
  |> (v => Console.log((v, v == 51 ? "pass" : "fail")));
  Expression.parse("2 * 3 + (4 * 5)")
  |> Expression.eval2
  |> (v => Console.log((v, v == 46 ? "pass" : "fail")));
  Expression.parse("5 + (8 * 3 + 9 + 3 * 4 * 3)")
  |> Expression.eval2
  |> (v => Console.log((v, v == 1445 ? "pass" : "fail")));
  Expression.parse("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
  |> Expression.eval2
  |> (v => Console.log((v, v == 669060 ? "pass" : "fail")));
  Expression.parse("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
  |> Expression.eval2
  |> (v => Console.log((v, v == 23340 ? "pass" : "fail")));
};

let run = () => {
  print_endline("---------- Day 18 ----------");
  /* testExpressions1(); */
  let input = Util.getLinesFromFile(path);

  let part1 = input |> List.map(parseEval) |> List.fold_left((+), 0);
  Console.log("Part 1> " ++ string_of_int(part1));

  /* testExpressions2(); */
  let part2 =
    input
    |> List.map(e => e |> Expression.parse |> Expression.eval2)
    |> List.fold_left((+), 0);
  Console.log("Part 1> " ++ string_of_int(part2));
};