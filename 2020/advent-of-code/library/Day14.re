open Angstrom;

let path = "./bin/input/input_day_14";
let testPath = "./bin/input/input_day_14_test";

module Input = {
  type mask =
    | X
    | I
    | O;
  type t =
    | Mask(array(mask))
    | Assign(int, int);

  let mask = t =>
    switch (t) {
    | Mask(m) => m
    | _ => failwith("invalid mask")
    };

  let integerParser =
    take_while1(
      fun
      | '0' .. '9' => true
      | _ => false,
    )
    >>| int_of_string;

  let assignmentParser =
    lift4(
      (_, addr, _, value) => Assign(addr, value),
      string("mem["),
      integerParser,
      string("] = "),
      integerParser,
    );

  let maskBitParser =
    take_while1(
      fun
      | 'X'
      | '1'
      | '0' => true
      | _ => false,
    );

  let maskParser =
    string("mask = ")
    *> maskBitParser
    >>| (
      s =>
        Util.stringToCharList(s)
        |> List.map(c =>
             switch (c) {
             | 'X' => X
             | '1' => I
             | '0' => O
             | _ => failwith("invalid char")
             }
           )
        |> (m => Mask(Array.of_list(m)))
    );

  let parser = assignmentParser <|> maskParser;

  let parse = s =>
    switch (parse_string(~consume=All, parser, s)) {
    | Ok(t) => t
    | Error(msg) => failwith(msg)
    };
};

module Instruction = {
  let dec2bin = n => {
    let bin = Array.make(36, 0);
    let rec aux = (index, remaining) =>
      if (remaining == 0) {
        bin;
      } else {
        bin[index] = remaining mod 2;
        aux(index - 1, remaining / 2);
      };
    aux(35, n);
  };

  let bin2dec = bs => {
    let len = (bs |> Array.length) - 1;
    bs
    |> Array.mapi((i, b) => b * int_of_float(2.0 ** float_of_int(len - i)))
    |> Array.fold_left((+), 0);
  };

  let applyMask = (mask, value) => {
    Array.map2(
      (maskBit, valueBit) =>
        switch (maskBit) {
        | Input.X => valueBit
        | Input.I => 1
        | Input.O => 0
        },
      mask,
      dec2bin(value),
    );
  };

  let eval = (memory, mask, instr) => {
    switch (instr) {
    | Input.Assign(m, v) =>
      memory[m] = applyMask(mask, v) |> bin2dec;
      (memory, mask);
    | Input.Mask(m) => (memory, m)
    };
  };
};

let run = () => {
  print_endline("---------- Day 14 ----------");
  let memory = Array.make(1000000, 0);

  let instructions =
    Util.getLinesFromFile(path) |> List.rev |> List.map(Input.parse);

  let mask = instructions |> List.hd |> Input.mask;
  let instrs = instructions |> List.tl;

  let answer =
    instrs
    |> List.fold_left(
         ((memory, mask), instr) => Instruction.eval(memory, mask, instr),
         (memory, mask),
       )
    |> (((memory, _)) => memory)
    |> Array.fold_left((+), 0);
  Console.log("Part 2> " ++ string_of_int(answer));
};