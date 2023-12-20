let read_file filename =
  let ic = open_in filename in
  let rec aux () =
    try
      let line = input_line ic in
      line :: aux ()
    with _ -> []
  in
  aux ()

type color = Red | Green | Blue

let string_of_color = function
  | Red -> "red"
  | Green -> "green"
  | Blue -> "blue"

let color_of_string = function
  | "red" -> Red
  | "green" -> Green
  | "blue" -> Blue
  | _ -> failwith "Invalid color"

type cube = int * color

let print_cube (n, color) =
  Printf.printf "| %d %s |" n (string_of_color color)

type subset_cube = cube list

let print_of_subset cubes =
  Printf.printf "[";
  List.iter print_cube cubes;
  Printf.printf "] ";

type game = {
  id : int;
  subsets : subset_cube list
}

let print_game { id ; subsets } =
  Printf.printf "Game %d\nSubsets: " id;
  List.iter print_of_subset subsets;
  Printf.printf "\n"

let cube_of_string s =
  match String.split_on_char ' ' s with
  | [ n ; color] -> int_of_string n , color_of_string color
  | _ -> failwith "Invalid cube"

let parse line =
  match String.split_on_char ':' line with
  | [ game ; cubes ] -> 
    let id = 
      match String.split_on_char ' ' game with 
      | [ "Game" ; n ] -> int_of_string n
      | _ -> failwith "Invalid game number"
    in
    let subsets = String.split_on_char ';' cubes in
    let subsets = List.map (fun subset ->
      let cubes = String.split_on_char ',' subset in
      let cubes = List.map (fun cube ->
        let cube = String.trim cube in
        cube_of_string cube  
      ) cubes in
      cubes
    ) subsets in
    (* print_of_game { id ; subsets }; *)
    (* is_game_valid { id ; subsets }; *)
    { id ; subsets }
  | _ -> failwith "Parse Error"

let criteria = (12, Red), (13, Green), (14, Blue)

let filter_cubes game color' =
  List.concat_map (fun subset -> 
    List.filter (fun (n, color) -> color = color') subset) game.subsets

let max_cube (n, c) (n', c') =
  let () = assert (c = c') in
  if n > n' then n, c else n', c'

let min_cube (n, c) (n', c') =
  let () = assert (c = c') in
  if n < n' then n, c else n', c'  

let max_min_cubes max_min cubes =
  match cubes with
  | [] -> failwith "empty cubes"
  | [ cube ] -> cube
  | cube :: cubes -> List.fold_left max_min cube cubes

let max_cubes = max_min_cubes max_cube
let min_cubes = max_min_cubes min_cube

let is_game_valid game =
  let red, _ = filter_cubes game Red |> max_cubes in
  let green, _ = filter_cubes game Green |> max_cubes in
  let blue, _ = filter_cubes game Blue |> max_cubes in
  red <= 12 && green <= 13 && blue <= 14

let power game =
  let red, _ = filter_cubes game Red |> max_cubes in
  let green, _ = filter_cubes game Green |> max_cubes in
  let blue, _ = filter_cubes game Blue |> max_cubes in
  red * green * blue

let main () =
  Printf.printf "==== Day 02 ====\n";
  (* let input = "inputs/day02_simpl.txt" in *)
  let input = "inputs/day02.txt" in
  let lines = read_file input in
  let games = List.map parse lines in
  let valid_games = List.filter is_game_valid games in
  let sum_game_ids = List.fold_left 
    (fun g ({ id ; subsets=_ }) -> g + id) 0 valid_games in
  Printf.printf "Part1> %d\n" sum_game_ids;
  let power = List.map power games |> List.fold_left ( + ) 0 in
  Printf.printf "Part2> %d\n" power;
  ()

let () = main()