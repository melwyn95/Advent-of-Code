let read_file filename =
  let ic = open_in filename in
  let rec aux () =
    try
      let line = input_line ic in
      line :: aux ()
    with _ -> []
  in
  aux ()

let rec zip xs ys =
  match xs, ys with
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: zip xs ys
  | _ -> failwith "unable to zip"

let parse lines = 
  match lines with
  | [ time ; distance ] ->
    let time = List.hd @@ List.tl @@ String.split_on_char ':' time in
    let time = String.trim time in
    let time = String.split_on_char ' ' time 
      |> List.filter (fun s -> s <> "") in
    let time = List.map int_of_string time in
    let distance = List.hd @@ List.tl @@ String.split_on_char ':' distance in
    let distance = String.trim distance in
    let distance = String.split_on_char ' ' distance 
      |> List.filter (fun s -> s <> "") in
    let distance = List.map int_of_string distance in
    zip time distance
  | _ -> failwith "invalid input"

let rec range start end_ =
  if start > end_
  then []
  else start :: range (start + 1) end_

let beat_record (time, distance) =
  let rec aux x =
    if (time - x) * x <= distance
    then 0
    else 1 + aux (x - 1)
  in
  Printf.printf "? %d\n" (aux (time / 2));
  2 * aux (time / 2) - if time mod 2 = 0 then 1 else 0
  (* List.length @@ List.filter_map (fun x ->
    if (time - x) * x > distance
    then Some x
    else None
  ) (range 0 time) *)

let main () =
  Printf.printf "==== Day 06 ====\n";
  (* let input = "inputs/day06_simpl.txt" in *)
  let input = "inputs/day06.txt" in
  let lines = read_file input in
  let rounds = parse lines in
  let error_margin = List.fold_left (fun e round ->
    e * beat_record round
  ) 1 rounds in
  Printf.printf "Part1> %d\n" error_margin;
  ()

let () = main ()