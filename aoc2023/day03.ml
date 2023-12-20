let read_file filename =
  let ic = open_in filename in
  let rec aux () =
    try
      let line = input_line ic in
      line :: aux ()
    with _ -> []
  in
  aux () 

type cell = 
    Unknown
  | Symbol of char
  | Number of int
  | Dot

let print_cell = function
  | Unknown -> Printf.printf "?"
  | Symbol c -> Printf.printf "%c" c
  | Number n -> Printf.printf "%d" n
  | Dot -> Printf.printf "."

let parse_number line =
  let rec aux line acc =
    let length = String.length line in
    if length = 0
    then int_of_string acc, ""
    else
      let ch = String.get line 0 in
      match ch with
      | '0' .. '9' -> aux (String.sub line 1 (length - 1)) 
                          (acc ^ String.make 1 ch)
      | _  -> int_of_string acc, line
  in
  aux line ""

let rec parse_line line =
  let length = String.length line in
  if length = 0
  then []
  else
    let ch = String.get line 0 in
    match ch with
    | '.' -> Dot :: parse_line (String.sub line 1 (length - 1))
    | '0' .. '9' ->
      let n, line = parse_number line in
      Number n :: parse_line line
    | ch -> Symbol ch :: parse_line (String.sub line 1 (length - 1))

let parse lines grid =
  List.iteri (fun i line -> 
    List.iteri (fun j cell -> grid.(i).(j) <- cell) (parse_line line)
  ) lines

let print_grid grid =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      print_cell cell;
      Printf.printf " ";  
    ) row;
    Printf.printf "\n"
  ) grid

let main () =
  Printf.printf "==== Day 03 ====\n";
  let input = "inputs/day03_simpl.txt" in
  (* let input = "inputs/day03.txt" in *)
  let lines = read_file input in
  let n = List.length lines in
  let grid = Array.make_matrix n n Unknown in
  let () = parse lines grid in
  print_grid grid;
  ()

let () = main()