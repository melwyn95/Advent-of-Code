
let read_file filename =
  let ic = open_in filename in
  let rec aux () =
    try
      let line = input_line ic in
      line :: aux ()
    with _ -> []
  in
  aux ()

let find_digit s =
  let length = String.length s in
  let rec aux idx =
    if idx < length
    then match String.get s idx with
    | '0' .. '9' -> int_of_char (String.get s idx) - int_of_char '0'
    | _ -> aux (idx + 1)
    else raise (Invalid_argument ("invalid index: " ^ s))
  in
  aux 0

let reverse_string s =
  String.fold_left (fun xs c -> c :: xs) [] s |> List.to_seq |> String.of_seq

let rec find_calibraion_values lines =
  match lines with
  | [] -> []
  | line :: lines -> find_digit line * 10 + find_digit (reverse_string line) 
                      :: find_calibraion_values lines

let sum values = List.fold_left (+) 0 values

let conv s ~reverse =
  let sub s len = 
    if reverse 
      then reverse_string (String.sub s 0 len) 
      else String.sub s 0 len
  in
  let sub_ s n len =
    if len - 5 >= 0
    then String.sub s n (len - n)
    else ""
  in
  let conv_prefix s =
    (* Printf.printf "len = %d | s = %s \n" (String.length s) s; *)
    let len = String.length s in
    let s = 
      if len >= 5
      then match sub s 5 with
      | "three" -> "3" ^ sub_ s 5 len
      | "seven" -> "7" ^ sub_ s 5 len
      | "eight" -> "8" ^ sub_ s 5 len
      | _       -> s
      else s
    in
    (* Printf.printf "5 len = %d | s = %s \n" (String.length s) s; *)
    let s = 
      if len >= 4
      then match sub s 4 with
      | "four" -> "4" ^ sub_ s 4 len
      | "five" -> "5" ^ sub_ s 4 len
      | "nine" -> "9" ^ sub_ s 4 len
      | _       -> s
      else s
    in
    (* Printf.printf "4 len = %d | s = %s \n" (String.length s) s; *)
    let s = 
      if len >= 3
      then match sub s 3 with
      | "one" -> "1" ^ sub_ s 3 len
      | "two" -> "2" ^ sub_ s 3 len
      | "six" -> "6" ^ sub_ s 3 len
      | _       -> s
      else s
    in
    (* Printf.printf "3 len = %d | s = %s \n" (String.length s) s; *)
    s
  in
  let rec aux s =
    Printf.printf "aux %s\n" s;
    if String.length s > 1
    then 
      let s = conv_prefix s in
      (* print_endline s; *)
      String.sub s 0 1 ^ (
        if String.length s > 0 
        then aux (String.sub s 1 (String.length s - 1))
        else ""
      )
    else s
  in
  (* print_endline s; *)
  (* print_endline (conv_prefix s); *)
  aux s

  let rec find_calibraion_values2 lines =
    match lines with
    | [] -> []
    | line :: lines ->  
      let num = find_digit (conv line ~reverse:false) * 10 + 
                find_digit (conv ~reverse:true (reverse_string line)) in
      Printf.printf "num = %d\n" num;
      num :: find_calibraion_values2 lines
  
  
let main () =
  Printf.printf "==== Day 01 ====\n";
  let input = "inputs/day01_simpl.txt" in
  (* let input = "inputs/day01.txt" in *)
  let lines = read_file input in
  (* let part1 = sum (find_calibraion_values lines) in *)
  (* Printf.printf "Part1> %d\n" part1; *)
  let part2 = sum (find_calibraion_values2 lines) in
  Printf.printf "Part2> %d\n" part2;
  ()

  let _ = main()