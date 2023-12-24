type range = {
  destination_start : int;
  source_start      : int;
  length            : int;
}

let print_range { destination_start ; source_start ; length } =
  Printf.printf "destination_start = %d; source_start = %d; length = %d\n"
    destination_start source_start length

let read_file filename =
  let ic = open_in filename in
  let rec aux () =
    try
      let line = input_line ic in
      line :: aux ()
    with _ -> []
  in
  aux ()

let split lines =
  let seeds = List.hd lines in
  let seeds =
    let parts = String.split_on_char ':' seeds in
    let parts = List.hd @@ List.tl parts in
    let parts = String.trim parts in
    let parts = String.split_on_char ' ' parts in
    List.map int_of_string parts
  in
  let maps = List.tl lines in
  let rec aux maps acc1 acc2 =
    match maps with
    | [] -> List.rev (acc1 :: acc2)
    | "" :: maps -> aux maps [] ((List.rev acc1) :: acc2)
    | line :: maps when String.contains line ':' -> aux maps acc1 acc2
    | line :: maps -> aux maps (line :: acc1) acc2
  in
  seeds, List.tl @@ aux maps [] []

let parse_maps : string list list -> range list list = fun maps ->
  List.map (fun map ->
    List.fold_right (fun line rs ->
      match String.split_on_char ' ' line with
      | [ d ; s ; l ] -> { destination_start = int_of_string d ;
                           source_start      = int_of_string s ;
                           length            = int_of_string l } :: rs
      | _ -> failwith "incorrect range format"
    ) map []
  ) maps

let find_destination value map =
  let { destination_start ; source_start ; length } = map in
  if value < source_start || value >= source_start + length
  then None
  else
    let diff = abs (value - source_start) in
    Some (destination_start + diff)

let find_destination maps value =
  match List.find_map (find_destination value) maps with
  | None -> value
  | Some value -> value

let find_location seed maps =
  List.fold_left (fun seed ranges ->
    find_destination ranges seed  
  ) seed maps   

let find_min_location seeds maps =
  match seeds with
  | seed :: seeds ->
    let location = find_location seed maps in
    List.fold_left (fun min_loc seed -> 
      min min_loc (find_location seed maps)
    ) location seeds
  | _ -> failwith "empty seeds"

let main () =
  Printf.printf "==== Day 05 ====\n";
  (* let input = "inputs/day05_simpl.txt" in *)
  let input = "inputs/day05.txt" in
  let lines = read_file input in
  let seeds, maps = split lines in
  let maps = parse_maps maps in
  let location = find_min_location seeds maps in
  Printf.printf "Part1> %d\n" location;
  (* List.iter (fun rs -> if rs = [] then Printf.printf "[]\n";List.iter (print_range) rs) maps; *)
  ()

let () = main ()