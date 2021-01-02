open Angstrom;

/* Logical XOR */
let (<<>>) = Util.(<<>>);
module Password = {
  type t = {
    lower: int,
    upper: int,
    chr: char,
    passwd: string,
  };

  let integerParser =
    take_while1(
      fun
      | '0' .. '9' => true
      | _ => false,
    )
    >>| int_of_string;

  let stringParser =
    take_while1(
      fun
      | '0' .. '9'
      | ' ' => false
      | _ => true,
    );

  let parser = {
    let lower = integerParser;
    let upper = char('-') *> integerParser;
    let chr = char(' ') *> take(1) >>| (s => s.[0]) <* string(": ");
    let passwd = stringParser;
    lift4(
      (lower, upper, chr, passwd) => {lower, upper, chr, passwd},
      lower,
      upper,
      chr,
      passwd,
    );
  };

  let parse = password => {
    switch (parse_string(~consume=All, parser, password)) {
    | Ok(p) => p
    | Error(msg) => failwith(msg)
    };
  };
};

let isPasswordValid: Password.t => bool =
  ({lower, upper, chr, passwd}) => {
    Util.stringToCharList(passwd)
    |> List.filter(c => c === chr)
    |> List.length
    |> (l => l >= lower && l <= upper);
  };

let isPasswordValidPart2: Password.t => bool =
  ({lower, upper, chr, passwd}) => {
    let lowerChar = passwd.[lower - 1];
    let upperChar = passwd.[upper - 1];

    lowerChar === chr <<>> (upperChar === chr);
  };

let run = () => {
  print_endline("---------- Day 2 ----------");
  /* Part 1 */
  let path = "./bin/input/input_day_2";
  let validPasswords =
    Util.getLinesFromFile(path)
    |> List.map(Password.parse)
    |> List.filter(isPasswordValid)
    |> List.length;

  Console.log(
    "Part 1> " ++ string_of_int(validPasswords),
  );
  /* Part 2 */
  let path = "./bin/input/input_day_2";
  let validPasswords =
    Util.getLinesFromFile(path)
    |> List.map(Password.parse)
    |> List.filter(isPasswordValidPart2)
    |> List.length;

  Console.log(
    "Part 2> " ++ string_of_int(validPasswords),
  );
};