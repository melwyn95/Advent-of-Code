open Angstrom;

module Passport = {
  type t = {
    byr: option(string), /* (Birth Year) */
    iyr: option(string), /* (Issue Year) */
    eyr: option(string), /* (Expiration Year) */
    hgt: option(string), /* (Height) */
    hcl: option(string), /* (Hair Color) */
    ecl: option(string), /* (Eye Color) */
    pid: option(string), /* (Passport ID) */
    cid: option(string) /* (Country ID) */
  };

  let toString = t =>
    [t.byr, t.iyr, t.eyr, t.hgt, t.hcl, t.ecl, t.pid, t.cid]
    |> List.fold_left(
         (acc, x) => acc ++ Option.value(x, ~default="<na>"),
         "",
       );

  let make =
      (
        ~byr=None,
        ~iyr=None,
        ~eyr=None,
        ~hgt=None,
        ~hcl=None,
        ~ecl=None,
        ~pid=None,
        ~cid=None,
        (),
      ) => {
    byr,
    iyr,
    eyr,
    hgt,
    hcl,
    ecl,
    pid,
    cid,
  };

  let merge = (p1: t, p2: t) => {
    switch (p2) {
    | {
        byr: Some(byr),
        iyr: None,
        eyr: None,
        hgt: None,
        hcl: None,
        ecl: None,
        pid: None,
        cid: None,
      } => {
        ...p1,
        byr: Some(byr),
      }
    | {
        byr: None,
        iyr: Some(iyr),
        eyr: None,
        hgt: None,
        hcl: None,
        ecl: None,
        pid: None,
        cid: None,
      } => {
        ...p1,
        iyr: Some(iyr),
      }
    | {
        byr: None,
        iyr: None,
        eyr: Some(eyr),
        hgt: None,
        hcl: None,
        ecl: None,
        pid: None,
        cid: None,
      } => {
        ...p1,
        eyr: Some(eyr),
      }
    | {
        byr: None,
        iyr: None,
        eyr: None,
        hgt: Some(hgt),
        hcl: None,
        ecl: None,
        pid: None,
        cid: None,
      } => {
        ...p1,
        hgt: Some(hgt),
      }
    | {
        byr: None,
        iyr: None,
        eyr: None,
        hgt: None,
        hcl: Some(hcl),
        ecl: None,
        pid: None,
        cid: None,
      } => {
        ...p1,
        hcl: Some(hcl),
      }
    | {
        byr: None,
        iyr: None,
        eyr: None,
        hgt: None,
        hcl: None,
        ecl: Some(ecl),
        pid: None,
        cid: None,
      } => {
        ...p1,
        ecl: Some(ecl),
      }
    | {
        byr: None,
        iyr: None,
        eyr: None,
        hgt: None,
        hcl: None,
        ecl: None,
        pid: Some(pid),
        cid: None,
      } => {
        ...p1,
        pid: Some(pid),
      }
    | {
        byr: None,
        iyr: None,
        eyr: None,
        hgt: None,
        hcl: None,
        ecl: None,
        pid: None,
        cid: Some(cid),
      } => {
        ...p1,
        cid: Some(cid),
      }

    | _ => p1
    };
  };

  let space = string(" ");
  let birthYear =
    string("byr:")
    *> take_while1(c => c != ' ')
    >>| (byr => make(~byr=Some(byr), ()));
  let issueYear =
    string("iyr:")
    *> take_while1(c => c != ' ')
    >>| (iyr => make(~iyr=Some(iyr), ()));
  let expirationYear =
    string("eyr:")
    *> take_while1(c => c != ' ')
    >>| (eyr => make(~eyr=Some(eyr), ()));
  let height =
    string("hgt:")
    *> take_while1(c => c != ' ')
    >>| (hgt => make(~hgt=Some(hgt), ()));
  let hairColor =
    string("hcl:")
    *> take_while1(c => c != ' ')
    >>| (hcl => make(~hcl=Some(hcl), ()));
  let eyeColor =
    string("ecl:")
    *> take_while1(c => c != ' ')
    >>| (ecl => make(~ecl=Some(ecl), ()));
  let passportId =
    string("pid:")
    *> take_while1(c => c != ' ')
    >>| (pid => make(~pid=Some(pid), ()));
  let countryId =
    string("cid:")
    *> take_while1(c => c != ' ')
    >>| (cid => make(~cid=Some(cid), ()));

  let parser = {
    let partialPassports =
      sep_by(
        space,
        birthYear
        <|> issueYear
        <|> expirationYear
        <|> height
        <|> height
        <|> hairColor
        <|> eyeColor
        <|> passportId
        <|> countryId
        <|> return(make()),
      );
    partialPassports;
  };

  let parse = passportStr => {
    switch (parse_string(~consume=All, parser, passportStr)) {
    | Ok(p) => p
    | Error(msg) => failwith(msg)
    };
  };

  let isPassportValid = p =>
    switch (p) {
    | {
        byr: Some(_),
        iyr: Some(_),
        eyr: Some(_),
        hgt: Some(_),
        hcl: Some(_),
        ecl: Some(_),
        pid: Some(_),
        cid: _,
      } =>
      true
    | _ => false
    };

  let isValidYear = (y, s, e) => {
    switch (int_of_string_opt(y)) {
    | Some(year) => year >= s && year <= e
    | None => false
    };
  };

  let isValidEyeColor = e =>
    switch (e) {
    | "amb"
    | "blu"
    | "brn"
    | "gry"
    | "grn"
    | "hzl"
    | "oth" => true
    | _ => false
    };

  let isValidPassportID = pid =>
    String.length(pid) == 9
    && (
      switch (int_of_string_opt(pid)) {
      | Some(_) => true
      | None => false
      }
    );

  let isValidHexChar = c =>
    switch (c) {
    | '0' .. '9'
    | 'a' .. 'f' => true
    | _ => false
    };

  let isValidHairColor = h =>
    String.length(h) == 7
    && h
    |> Util.stringToCharList
    |> List.tl
    |> List.filter(isValidHexChar)
    |> List.length == 6;

  let isValidHeight = ht => {
    let strlen = String.length(ht);
    let digits = String.sub(ht, 0, strlen - 2);
    let unit = String.sub(ht, strlen - 2, 2);

    if (unit == "cm") {
      switch (int_of_string_opt(digits)) {
      | Some(h) => h >= 150 && h <= 193
      | None => false
      };
    } else if (unit == "in") {
      switch (int_of_string_opt(digits)) {
      | Some(h) => h >= 59 && h <= 76
      | None => false
      };
    } else {
      false;
    };
  };

  let isPassportValid2 = p => {
    switch (p) {
    | {
        byr: Some(byr),
        iyr: Some(iyr),
        eyr: Some(eyr),
        hgt: Some(hgt),
        hcl: Some(hcl),
        ecl: Some(ecl),
        pid: Some(pid),
        cid: _,
      } =>
      isValidYear(byr, 1920, 2002)
      && isValidYear(iyr, 2010, 2020)
      && isValidYear(eyr, 2020, 2030)
      && isValidHeight(hgt)
      && isValidHairColor(hcl)
      && isValidEyeColor(ecl)
      && isValidPassportID(pid)
    | _ => false
    };
  };
};
let prepareInput = path => {
  Util.getLinesFromFile(path)
  |> List.fold_left(
       ((xs, str), x) =>
         if (x == "") {
           (List.cons(str, xs), "");
         } else {
           (xs, str ++ x ++ " ");
         },
       ([], ""),
     )
  |> (((xs, str)) => List.cons(str, xs));
};

let path = "./bin/input_day_4";
let testPath = "./bin/input_day_4_test";

let run = () => {
  print_endline("---------- Day4 ----------");

  let assemblePassport = p =>
    p
    |> Passport.parse
    |> List.fold_left((acc, p) => Passport.merge(acc, p), Passport.make());

  let inputPart1 = prepareInput(path);
  let validPassports =
    inputPart1
    |> List.map(assemblePassport)
    |> List.filter(Passport.isPassportValid);

  let numberValidPassports = validPassports |> List.length;

  Console.log("Part 1> " ++ string_of_int(numberValidPassports));

  let validPassports2 =
    inputPart1
    |> List.map(assemblePassport)
    |> List.filter(Passport.isPassportValid2);

  let numberValidPassports2 = validPassports2 |> List.length;

  Console.log("Part 2> " ++ string_of_int(numberValidPassports2));
};