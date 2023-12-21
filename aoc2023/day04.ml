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

let update_counts idx card_counts n =
  for i = idx + 1 to idx + n do
    if i >= Array.length card_counts then () else
    card_counts.(i) <- card_counts.(i) + card_counts.(idx) 
  done

let find_matching card_counts i card =
  let matching = IntSet.inter card.winning card.existing in
  if matching = IntSet.empty 
  then 0 
  else
    let n = IntSet.cardinal matching in
    update_counts i card_counts n;
    power 2 (n - 1)

let main () =
  Printf.printf "==== Day 04 ====\n";
  (* let input = "inputs/day04_simpl.txt" in *)
  let input = "inputs/day04.txt" in
  let lines = read_file input in
  let cards = List.map parse lines in
  let card_counts = Array.make (List.length cards) 1 in
  let points = List.mapi (find_matching card_counts) cards 
    |> List.fold_left ( + ) 0 in
  Printf.printf "Part1> %d\n" points;
  let total_cards = Array.fold_left ( + ) 0 card_counts in
  Printf.printf "Part2> %d\n" total_cards;
  ()

let () = main()