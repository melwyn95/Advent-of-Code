type t = {
  door: int,
  card: int,
};

let testInput = {door: 17807724, card: 5764801};
let input = {door: 8252394, card: 6269621};

let subjectNumber = 7;
let f = 20201227;

let reverseTransform = key => {
  let rec aux = (value, loopSize) =>
    if (value == key) {
      loopSize;
    } else {
      aux(value * subjectNumber mod f, loopSize + 1);
    };
  aux(1, 0);
};

let transform = (subjectNumber, loopSize) => {
  let rec aux = (value, loopSize) =>
    if (loopSize == 0) {
      value;
    } else {
      aux(value * subjectNumber mod f, loopSize - 1);
    };
  aux(1, loopSize);
};

let run = () => {
  print_endline("---------- Day 25 ----------");
  let input = input;
  let loopSizeCard = reverseTransform(input.card);
  /* let loopSizeDoor = reverseTransform(input.door); */

  let encryptionKey = transform(input.door, loopSizeCard);

  /* Console.log(("card", loopSizeCard));
     Console.log(("door", loopSizeDoor)); */
  Console.log("Part 1> " ++ string_of_int(encryptionKey));
};