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
  let first_digit s =
    let length = String.length s in
    if length < 3
    then String.sub s 0 1, String.sub s 1 (length - 1)
    else
      let sub = String.sub s 0 3 in
      let sub = if reverse then reverse_string sub else sub in
      match sub with
      | "one" -> "1", String.sub s 3 (length - 3)
      | "two" -> "2", String.sub s 3 (length - 3)
      | "six" -> "6", String.sub s 3 (length - 3)
      | _ ->
        if length < 4 then String.sub s 0 1, String.sub s 1 (length - 1) else
        let sub = String.sub s 0 4 in
        let sub = if reverse then reverse_string sub else sub in
        match sub with
        | "four" -> "4", String.sub s 4 (length - 4)
        | "five" -> "5", String.sub s 4 (length - 4)
        | "nine" -> "9", String.sub s 4 (length - 4)
        | _ ->
          if length < 5 then String.sub s 0 1, String.sub s 1 (length - 1) else
          let sub = String.sub s 0 5 in
          let sub = if reverse then reverse_string sub else sub in
          match sub with
          | "three" -> "3", String.sub s 5 (length - 5)
          | "seven" -> "7", String.sub s 5 (length - 5)
          | "eight" -> "8", String.sub s 5 (length - 5)
          | _ -> String.sub s 0 1, String.sub s 1 (length - 1)
  in
  let rec aux s =
    if String.length s < 3
    then s
    else
      let d, s = first_digit s in
      d ^ aux s
  in
  aux s

let rec find_calibraion_values2 lines =
  match lines with
  | [] -> []
  | line :: lines ->  
    let num = find_digit (conv line ~reverse:false) * 10 + 
              find_digit (conv ~reverse:true (reverse_string line)) in
    num :: find_calibraion_values2 lines

let main () =
  Printf.printf "==== Day 01 ====\n";
  (* let input = "inputs/day01_simpl.txt" in *)
  let input = "inputs/day01.txt" in
  let lines = read_file input in
  let part1 = sum (find_calibraion_values lines) in
  Printf.printf "Part1> %d\n" part1;
  let part2 = sum (find_calibraion_values2 lines) in
  Printf.printf "Part2> %d\n" part2;
  ()

  let () = main()