
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

type subset_cube = cube list

let print_of_subset cubes =
  Printf.printf "[";
  List.iter (fun (n, color) -> 
    Printf.printf "| %d %s |" n (string_of_color color)) cubes;
  Printf.printf "] ";

type game = {
  id : int;
  subsets : subset_cube list
}

let print_of_game { id ; subsets } =
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
    print_of_game { id ; subsets };
    { id ; subsets }
  | _ -> failwith "Parse Error"

let main () =
  Printf.printf "==== Day 02 ====\n";
  let input = "inputs/day02_simpl.txt" in
  (* let input = "inputs/day02.txt" in *)
  let lines = read_file input in
  let _ = List.map parse lines in
  ()

  let () = main()