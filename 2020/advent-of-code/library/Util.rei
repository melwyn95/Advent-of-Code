let hello: unit => string;

let getLinesFromFile: string => list(string);

let stringToCharList: string => list(char);

/* Logical XOR */
let (<<>>): (bool, bool) => bool;

let zipListOfLists: (list(list('a)), list(list('a))) => list(list('a));

let stringContains: (string, string) => bool;

let bin2dec: array(int) => int;

let dec2bin: int => array(int);

let keepDups: list('a) => list('a);