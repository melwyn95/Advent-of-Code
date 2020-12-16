open Angstrom;

let path = "./bin/input/input_day_16";
let testPath = "./bin/input/input_day_16_test";

module Field = {
  type range = {
    _start: int,
    _end: int,
  };
  type t = {
    name: string,
    startRange: range,
    endRange: range,
  };

  let integerParser =
    take_while(
      fun
      | '0' .. '9' => true
      | _ => false,
    )
    >>| int_of_string;

  let nameParser = take_while1(c => c != ':') <* string(": ");
  let rangeParser =
    lift3(
      (s, _, e) => {_start: s, _end: e},
      integerParser,
      char('-'),
      integerParser,
    );
  let orParser = string(" or ");

  let parser =
    lift4(
      (name, startRange, _, endRange) => {name, startRange, endRange},
      nameParser,
      rangeParser,
      orParser,
      rangeParser,
    );

  let parse = field =>
    switch (parse_string(~consume=All, parser, field)) {
    | Ok(t) => t
    | Error(msg) => failwith(msg)
    };
};

module Ticket = {
  type t = list(int);

  let parse = ticket =>
    ticket |> String.split_on_char(',') |> List.map(int_of_string);
};

let rec parseNearbyTickets = (tickets, nearbyTickets, yourTicket, fields) =>
  switch (tickets) {
  | [] => (fields, yourTicket, nearbyTickets)
  | ["nearby tickets:", ...tickets] =>
    parseNearbyTickets(tickets, nearbyTickets, yourTicket, fields)
  | [ticket, ...tickets] =>
    parseNearbyTickets(
      tickets,
      List.cons(Ticket.parse(ticket), nearbyTickets),
      yourTicket,
      fields,
    )
  };

let rec parseYourTicket = (tickets, fields) =>
  switch (tickets) {
  | ["your ticket:", ticket, "", ...next] =>
    parseNearbyTickets(next, [], Ticket.parse(ticket), fields)
  | []
  | _ => failwith("parser failed: your ticket")
  };

let rec parseFields = (fields, parsedFields) =>
  switch (fields) {
  | [] => failwith("parser failed: fields")
  | [field, "", ...next] =>
    parseYourTicket(next, field |> Field.parse |> List.cons(_, parsedFields))
  | [field, ...fields] =>
    parseFields(fields, field |> Field.parse |> List.cons(_, parsedFields))
  };

let isValueValidForField = (value, field) => {
  open Field;
  let {startRange, endRange} = field;
  value >= startRange._start
  && value <= startRange._end
  || value >= endRange._start
  && value <= endRange._end;
};

let rec isValueWithinRange = (value, fields) => {
  Field.(
    switch (fields) {
    | [] => false
    | [field, ...fields] =>
      isValueValidForField(value, field)
        ? true : isValueWithinRange(value, fields)
    }
  );
};

let rec findInvalidValues = (values, fields) => {
  values |> List.filter(value => !isValueWithinRange(value, fields));
};

let isValid = (value, fields) =>
  findInvalidValues(value, fields) |> (xs => List.length(xs) == 0);

let sumList = List.fold_left((+), 0);

let getColumns = xs => {
  let ys = List.hd(xs) |> List.map(x => [x]);
  let xs = List.tl(xs);

  List.fold_left(
    (ys, x) => List.map2((x, ys) => List.cons(x, ys), x, ys),
    ys,
    xs,
  );
};

let rec getApplicableField = (column, fields, validList) => {
  Field.(
    switch (fields) {
    | [] => validList
    | [field, ...fields] =>
      let isFieldValid =
        column
        |> List.filter(v => !isValueValidForField(v, field))
        |> (vs => List.length(vs) == 0);
      let vs = isFieldValid ? List.cons(field.name, validList) : validList;
      getApplicableField(column, fields, vs);
    }
  );
};

let getFieldsOrder = (fieldNamesCadidates, fieldNames) => {
  let (fieldNames, index) = List.hd(fieldNamesCadidates);
  let fieldName = List.hd(fieldNames);

  fieldNamesCadidates
  |> List.tl
  |> List.fold_left(
       (names, (candidates, index)) => {
         List.filter(
           candidate =>
             !List.exists(((name, _)) => name == candidate, names),
           candidates,
         )
         |> List.hd
         |> (n => List.cons((n, index), names))
       },
       [(fieldName, index)],
     )
  |> List.sort(((_, i), (_, j)) => Stdlib.compare(i, j))
  |> List.map(((n, _)) => n);
};

let run = () => {
  print_endline("---------- Day 16 ----------");
  let (fields, yourTicket, nearbyTickets) =
    Util.getLinesFromFile(path) |> List.rev |> parseFields(_, []);

  let invalidSum =
    nearbyTickets
    |> List.map(values => findInvalidValues(values, fields))
    |> List.fold_left(
         (sum, invalidValues) => sum + sumList(invalidValues),
         0,
       );

  Console.log("Part 1> " ++ string_of_int(invalidSum));

  let validTickets =
    nearbyTickets |> List.filter(values => isValid(values, fields));

  let columns = getColumns(validTickets);
  let fieldNameCandidates =
    columns
    |> List.mapi((i, column) =>
         (getApplicableField(column, fields, []), i)
       )
    |> List.sort(((x, _), (y, _)) =>
         Stdlib.compare(List.length(x), List.length(y))
       );

  let fieldNames = fields |> List.map(field => Field.(field.name));

  let correctOrderFieldNames =
    getFieldsOrder(fieldNameCandidates, fieldNames);

  List.map2(
    (name, value) =>
      Util.stringContains(name, "departure") ? Some(value) : None,
    correctOrderFieldNames,
    yourTicket,
  )
  |> List.filter_map(x => x)
  |> List.fold_left(( * ), 1)
  |> (n => Console.log("Part 2> " ++ string_of_int(n)));
};