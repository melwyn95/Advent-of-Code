let read_file filename =
  let ic = open_in filename in
  let rec aux () =
    try
      let line = input_line ic in
      line :: aux ()
    with _ -> []
  in
  aux ()

module IntSet = Set.Make (Int)

type card = {
  id : int ;
  winning : IntSet.t ;
  existing : IntSet.t ;
}

let parse line =
  match String.split_on_char ':' line with
  | [ card ; numbers ] -> 
    let id = int_of_string 
      (List.hd @@ List.rev @@ String.split_on_char ' ' card)
    in
    let winning, existing = 
      match String.split_on_char '|' numbers with
      | [ winning ; existing ] ->
        let winning = String.trim winning 
          |> String.split_on_char ' '
          |> List.filter (fun n -> n <> "")
          |> List.map int_of_string
          |> List.to_seq
          |> IntSet.of_seq
        in
        let existing = String.trim existing 
          |> String.split_on_char ' '
          |> List.filter (fun n -> n <> "")
          |> List.map int_of_string
          |> List.to_seq
          |> IntSet.of_seq
        in
        winning, existing
      | _ -> failwith "Invalid card numbers" 
    in
    { id ; winning ; existing }
  | _ -> failwith "Unable to split card"

let power x n =
  let rec aux n acc =
    if n = 0
    then acc
    else if n mod 2 = 1
    then x * aux (n - 1) acc
    else let y = aux (n / 2) acc in y * y
  in
  aux n 1

let find_matching card =
  let matching = IntSet.inter card.winning card.existing in
  if matching = IntSet.empty 
  then 0 
  else power 2 (IntSet.cardinal matching - 1)

let main () =
  Printf.printf "==== Day 04 ====\n";
  (* let input = "inputs/day04_simpl.txt" in *)
  let input = "inputs/day04.txt" in
  let lines = read_file input in
  let cards = List.map parse lines in
  let points = List.map find_matching cards |> List.fold_left ( + ) 0 in
  Printf.printf "Part1> %d\n" points;
  ()

let () = main()