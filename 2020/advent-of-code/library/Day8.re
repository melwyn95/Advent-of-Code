open Angstrom;

module Instruction = {
  type t =
    | Nop(int)
    | Acc(int)
    | Jmp(int);

  let signedInteger =
    fun
    | '0' .. '9'
    | '+'
    | '-' => true
    | _ => false;

  let accParser =
    string("acc ")
    *> take_while1(signedInteger)
    >>| (n => Acc(int_of_string(n)));
  let jmpParser =
    string("jmp ")
    *> take_while1(signedInteger)
    >>| (n => Jmp(int_of_string(n)));
  let nopParser =
    string("nop ")
    *> take_while1(signedInteger)
    >>| (n => Nop(int_of_string(n)));

  let parser = accParser <|> jmpParser <|> nopParser;

  let parse = instruction =>
    switch (parse_string(~consume=All, parser, instruction)) {
    | Ok(t) => t
    | Error(msg) => failwith(msg)
    };

  let execute = (ip, acc, t) =>
    switch (t) {
    | Nop(_) => (ip + 1, acc)
    | Acc(n) => (ip + 1, acc + n)
    | Jmp(n) => (ip + n, acc)
    };

  let flip = instruction =>
    switch (instruction) {
    | Nop(n) => Jmp(n)
    | Jmp(n) => Nop(n)
    | _ => instruction
    };
};

module IpSet = Set.Make(Int);

let rec processor = (ip, acc, instructions, memo) => {
  let (i, instruction) = instructions |> List.find(((i, _)) => i == ip);
  IpSet.mem(i, memo)
    ? (false, ip, acc)
    : {
      let (nextIp, nextAcc) = Instruction.execute(ip, acc, instruction);
      try(processor(nextIp, nextAcc, instructions, IpSet.add(ip, memo))) {
      | Not_found => (true, ip, nextAcc)
      };
    };
};

let rec loopDetector = (ip, acc, instructions, memo) => {
  let (i, instruction) = instructions |> List.find(((i, _)) => i == ip);
  IpSet.mem(i, memo)
    ? memo
    : {
      let (nextIp, nextAcc) = Instruction.execute(ip, acc, instruction);
      loopDetector(nextIp, nextAcc, instructions, IpSet.add(ip, memo));
    };
};

let getNewInstructions = (instructions, ip) =>
  instructions
  |> List.map(((i, instruction)) =>
       (i, i == ip ? Instruction.flip(instruction) : instruction)
     );

let testInstructions = (instructions, ip) => {
  let newInstructions = getNewInstructions(instructions, ip);
  let (didTerminate, _, acc) = processor(0, 0, newInstructions, IpSet.empty);
  (didTerminate, acc);
};

let rec findTerminatingInstructons = (instructions, ips) =>
  switch (ips) {
  | [] => (-1)
  | [ip, ...ips] =>
    switch (testInstructions(instructions, ip)) {
    | (true, acc) => acc
    | (false, _) => findTerminatingInstructons(instructions, ips)
    }
  };

let testPath = "./bin/input/input_day_8_test";
let path = "./bin/input/input_day_8";

let run = () => {
  print_endline("---------- Day 8 ----------");
  let instructions =
    Util.getLinesFromFile(path)
    |> List.rev
    |> List.mapi((i, x) => (i, Instruction.parse(x)));

  processor(0, 0, instructions, IpSet.empty)
  |> (((_, _, acc)) => Console.log("Part 1> " ++ string_of_int(acc)));

  loopDetector(0, 0, instructions, IpSet.empty)
  |> IpSet.elements
  |> findTerminatingInstructons(instructions)
  |> (n => Console.log("Part 2> " ++ string_of_int(n)));
};