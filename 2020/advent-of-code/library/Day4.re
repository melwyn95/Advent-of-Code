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
  type tVariant =
    | Byr(string)
    | Iyr(string)
    | Eyr(string)
    | Hgt(string)
    | Hcl(string)
    | Ecl(string)
    | Pid(string)
    | Cid(string);

  let make = () => {
    byr: None,
    iyr: None,
    eyr: None,
    hgt: None,
    hcl: None,
    ecl: None,
    pid: None,
    cid: None,
  };

  let rec merge = (acc, xs) => {
    switch (xs) {
    | [] => acc
    | [hd, ...tl] =>
      switch (hd) {
      | Byr(byr) => merge({...acc, byr: Some(byr)}, tl)
      | Iyr(iyr) => merge({...acc, iyr: Some(iyr)}, tl)
      | Eyr(eyr) => merge({...acc, eyr: Some(eyr)}, tl)
      | Hgt(hgt) => merge({...acc, hgt: Some(hgt)}, tl)
      | Hcl(hcl) => merge({...acc, hcl: Some(hcl)}, tl)
      | Ecl(ecl) => merge({...acc, ecl: Some(ecl)}, tl)
      | Pid(pid) => merge({...acc, pid: Some(pid)}, tl)
      | Cid(cid) => merge({...acc, cid: Some(cid)}, tl)
      }
    };
  };

  let space = char(' ');
  let birthYear =
    string("byr:") *> take_while1(c => c != ' ') >>| (byr => Byr(byr));
  let issueYear =
    string("iyr:") *> take_while1(c => c != ' ') >>| (iyr => Iyr(iyr));
  let expirationYear =
    string("eyr:") *> take_while1(c => c != ' ') >>| (eyr => Eyr(eyr));
  let height =
    string("hgt:") *> take_while1(c => c != ' ') >>| (hgt => Hgt(hgt));
  let hairColor =
    string("hcl:") *> take_while1(c => c != ' ') >>| (hcl => Hcl(hcl));
  let eyeColor =
    string("ecl:") *> take_while1(c => c != ' ') >>| (ecl => Ecl(ecl));
  let passportId =
    string("pid:") *> take_while1(c => c != ' ') >>| (pid => Pid(pid));
  let countryId =
    string("cid:") *> take_while1(c => c != ' ') >>| (cid => Cid(cid));

  let parser = {
    let partialPassports =
      sep_by1(
        space,
        birthYear
        <|> issueYear
        <|> expirationYear
        <|> height
        <|> height
        <|> hairColor
        <|> eyeColor
        <|> passportId
        <|> countryId,
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
           (xs, String.trim(str ++ " " ++ x));
         },
       ([], ""),
     )
  |> (((xs, str)) => List.cons(str, xs));
};

let path = "./bin/input_day_4";
let testPath = "./bin/input_day_4_test";

let run = () => {
  print_endline("---------- Day 4 ----------");

  let assemblePassport = p =>
    p |> Passport.parse |> Passport.merge(Passport.make());

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