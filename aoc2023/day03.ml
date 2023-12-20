let read_file filename =
  let ic = open_in filename in
  let rec aux () =
    try
      let line = input_line ic in
      line :: aux ()
    with _ -> []
  in
  aux () 

module NumberSet = Set.Make (struct
  type t = int * (int * int)
  let compare (n1, (x1, y1)) (n2, (x2, y2)) =
    match Int.compare x1 x2 with
    | 0 -> 
      (match Int.compare y1 y2 with
      | 0 -> Int.compare n1 n2
      | c -> c)
    | c -> c
end)

type cell = 
    Unknown
  | Symbol of char
  | Number of int * (int * int)
  | Dot

let print_cell = function
  | Unknown -> Printf.printf "?"
  | Symbol c -> Printf.printf "%c" c
  | Number (n, _) -> Printf.printf "%d" n
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
      | _ -> int_of_string acc, line
  in
  aux line ""

let rec digits n =
  if n >= 0 && n <= 9
  then [ n ]
  else (n mod 10) :: digits (n / 10)

let repeat n x =
  let rec aux n =
    if n >= 0 && n <= 9
    then [ x ]
    else x :: aux (n / 10)
  in
  aux n

let rec parse_line i j line =
  let length = String.length line in
  if length = 0
  then []
  else
    let ch = String.get line 0 in
    match ch with
    | '.' -> Dot :: parse_line i (j + 1) (String.sub line 1 (length - 1))
    | '0' .. '9' ->
      let n, line = parse_number line in
      repeat n (Number (n, (i, j))) 
        @ parse_line i (j + List.length (digits n)) line
    | ch -> Symbol ch :: parse_line i (j + 1) (String.sub line 1 (length - 1))

let parse lines grid =
  List.iteri (fun i line ->
    List.iteri (fun j cell -> grid.(i).(j) <- cell) (parse_line i 0 line)
  ) lines

let print_grid grid =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      print_cell cell;
      Printf.printf " ";  
    ) row;
    Printf.printf "\n"
  ) grid

let is_valid_index n m i j =
  i >= 0 && i < n && j >= 0 && j < m

let is_number = function Number _ -> true | _ -> false
let get_number = function Number (n, (i, j)) -> n, (i, j) | _ -> failwith "Not a number"

let find_neighbour_numbers n m i j grid =
  let neighbours = [] in
  let neighbours = 
    if is_valid_index n m (i - 1) j && is_number grid.(i - 1).(j)
    then get_number grid.(i - 1).(j) :: neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m (i - 1) (j - 1) && is_number grid.(i - 1).(j - 1)
    then get_number grid.(i - 1).(j - 1) :: neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m (i - 1) (j + 1) && is_number grid.(i - 1).(j + 1)
    then get_number grid.(i - 1).(j + 1) :: neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m i (j - 1) && is_number grid.(i).(j - 1)
    then get_number grid.(i).(j - 1) :: neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m i (j + 1) && is_number grid.(i).(j + 1)
    then get_number grid.(i).(j + 1) :: neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m (i + 1) j && is_number grid.(i + 1).(j)
    then get_number grid.(i + 1).(j) :: neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m (i + 1) (j - 1) && is_number grid.(i + 1).(j - 1)
    then get_number grid.(i + 1).(j - 1) :: neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m (i + 1) (j + 1) && is_number grid.(i + 1).(j + 1)
    then get_number grid.(i + 1).(j + 1) :: neighbours else neighbours 
  in
  neighbours

let find_part_numbers n m grid = 
  snd @@ Array.fold_left (fun (i, pos_set) row ->
    i + 1, snd @@ Array.fold_left (fun (j, pos_set) cell ->
      match cell with
      | Symbol _ ->
        let ns = find_neighbour_numbers n m i j grid |> List.to_seq in
        j + 1, NumberSet.add_seq ns pos_set
      | _ -> j + 1, pos_set
    ) (0, pos_set) row  
  ) (0, NumberSet.empty) grid

let find_gears n m i j grid =
  let neighbours = NumberSet.empty in
  let neighbours = 
    if is_valid_index n m (i - 1) j && is_number grid.(i - 1).(j)
    then NumberSet.add (get_number grid.(i - 1).(j)) neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m (i - 1) (j - 1) && is_number grid.(i - 1).(j - 1)
    then NumberSet.add (get_number grid.(i - 1).(j - 1)) neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m (i - 1) (j + 1) && is_number grid.(i - 1).(j + 1)
    then NumberSet.add (get_number grid.(i - 1).(j + 1)) neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m i (j - 1) && is_number grid.(i).(j - 1)
    then NumberSet.add (get_number grid.(i).(j - 1)) neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m i (j + 1) && is_number grid.(i).(j + 1)
    then NumberSet.add (get_number grid.(i).(j + 1)) neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m (i + 1) j && is_number grid.(i + 1).(j)
    then NumberSet.add (get_number grid.(i + 1).(j)) neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m (i + 1) (j - 1) && is_number grid.(i + 1).(j - 1)
    then NumberSet.add (get_number grid.(i + 1).(j - 1)) neighbours else neighbours 
  in
  let neighbours = 
    if is_valid_index n m (i + 1) (j + 1) && is_number grid.(i + 1).(j + 1)
    then NumberSet.add (get_number grid.(i + 1).(j + 1)) neighbours else neighbours 
  in
  if NumberSet.cardinal neighbours = 2 then
    match List.of_seq @@ NumberSet.to_seq neighbours with
    | [ (n1, _) ; (n2, _) ] -> Some (n1 * n2)
    | _ -> failwith "impossible"
  else None

let find_gear_ratios n m grid =
  snd @@ Array.fold_left (fun (i, gear_ratio_sum) row ->
    i + 1, snd @@ Array.fold_left (fun (j, gear_ratio_sum) cell ->
      match cell with
      | Symbol '*' ->
        let ns = find_gears n m i j grid in
        let gear_ratio_sum = 
          match ns with
          | Some gear_ratio -> gear_ratio_sum + gear_ratio
          | None -> gear_ratio_sum 
        in
        j + 1, gear_ratio_sum
      | _ -> j + 1, gear_ratio_sum
    ) (0, gear_ratio_sum) row  
  ) (0, 0) grid

let main () =
  Printf.printf "==== Day 03 ====\n";
  (* let input = "inputs/day03_simpl.txt" in *)
  let input = "inputs/day03.txt" in
  let lines = read_file input in
  let n = List.length lines in
  let grid = Array.make_matrix n n Unknown in
  let () = parse lines grid in
  (* print_grid grid; *)
  let ns = find_part_numbers n n grid in
  let sum = NumberSet.to_seq ns 
    |> List.of_seq 
    |> List.map (fun (n, (i, j)) -> n)
    |> List.fold_left ( + ) 0 
  in
  Printf.printf "Part1> %d\n" sum;
  let gear_ratio_sum = find_gear_ratios n n grid in
  Printf.printf "Part2> %d\n" gear_ratio_sum;
  ()

let () = main()