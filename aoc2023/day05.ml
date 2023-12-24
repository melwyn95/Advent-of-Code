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
  seeds, aux maps [] []

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

let main () =
  Printf.printf "==== Day 05 ====\n";
  let input = "inputs/day05_simpl.txt" in
  (* let input = "inputs/day05.txt" in *)
  let lines = read_file input in
  let seeds, maps = split lines in
  let maps = parse_maps maps in
  List.iter (List.iter (print_range)) maps;
  ()

let () = main ()