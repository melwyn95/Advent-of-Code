let hello: unit => string;

let getLinesFromFile: string => list(string);

let stringToCharList: string => list(char);

/* Logical XOR */
let (<<>>): (bool, bool) => bool;

let zipListOfLists: (list(list('a)), list(list('a))) => list(list('a));