open Angstrom;

let path = "./bin/input/input_day_14";
let testPath = "./bin/input/input_day_14_test";

type hashTableType = Hashtbl.t(int, int);

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

  let to_mask = i =>
    switch (i) {
    | 0 => O
    | 1 => I
    | _ => failwith("invalid bit")
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
  let applyMask = (mask, value) => {
    Array.map2(
      (maskBit, valueBit) =>
        switch (maskBit) {
        | Input.X => valueBit
        | Input.I => 1
        | Input.O => 0
        },
      mask,
      Util.dec2bin(value),
    );
  };

  let eval = (memory, mask, instr) => {
    switch (instr) {
    | Input.Assign(m, v) =>
      memory[m] = applyMask(mask, v) |> Util.bin2dec;
      (memory, mask);
    | Input.Mask(m) => (memory, m)
    };
  };

  let mask2bin = mask =>
    mask
    |> Array.map(a =>
         switch (a) {
         | Input.I => 1
         | Input.O => 0
         | _ => failwith("unresolved floating")
         }
       );

  let expandMask = mask => {
    let len = mask |> Array.length;
    let rec aux = (masks, index) =>
      if (index == len) {
        masks;
      } else {
        let newMasks =
          switch (mask[index]) {
          | Input.X =>
            let one =
              masks
              |> List.map(Array.copy)
              |> List.map(m => {
                   m[index] = Input.I;
                   m;
                 });
            let zero =
              masks
              |> List.map(m => {
                   m[index] = Input.O;
                   m;
                 });
            List.concat([one, zero]);
          | Input.I =>
            masks
            |> List.map(m => {
                 m[index] = Input.I;
                 m;
               })
          | Input.O =>
            masks
            |> List.map(m => {
                 m[index] = Input.O;
                 m;
               })
          };
        aux(newMasks, index + 1);
      };
    aux([Array.copy(mask)], 0);
  };

  let applyMask2 = (mask, address) => {
    Array.map2(
      (maskBit, addressBit) =>
        switch (maskBit) {
        | Input.X => Input.X
        | Input.I => Input.I
        | Input.O => Input.to_mask(addressBit)
        },
      mask,
      address,
    );
  };

  let eval2 = (memory, mask, instr) => {
    switch (instr) {
    | Input.Assign(m, v) =>
      let memoryLocations =
        applyMask2(mask, Util.dec2bin(m))
        |> expandMask
        |> List.map(mask2bin)
        |> List.map(Util.bin2dec);
      memoryLocations
      |> List.iter(loc =>
           if (Hashtbl.mem(memory, loc)) {
             Hashtbl.replace(memory, loc, v);
           } else {
             Hashtbl.add(memory, loc, v);
           }
         );
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
  Console.log("Part 1> " ++ string_of_int(answer));

  let memory = Hashtbl.create(1000);
  let answer =
    instrs
    |> List.fold_left(
         ((memory, mask), instr) => Instruction.eval2(memory, mask, instr),
         (memory, mask),
       )
    |> (((memory, _)) => Hashtbl.to_seq_values(memory))
    |> Seq.fold_left((+), 0);
  Console.log("Part 2> " ++ string_of_int(answer));
};