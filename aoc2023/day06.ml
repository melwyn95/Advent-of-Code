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

let parse2 lines = 
  match lines with
  | [ time ; distance ] ->
    let time = List.hd @@ List.tl @@ String.split_on_char ':' time in
    let time = String.trim time in
    let time = String.split_on_char ' ' time 
      |> List.filter (fun s -> s <> "") in
    let time = List.fold_left String.cat "" time in
    let distance = List.hd @@ List.tl @@ String.split_on_char ':' distance in
    let distance = String.trim distance in
    let distance = String.split_on_char ' ' distance 
      |> List.filter (fun s -> s <> "") in
    let distance = List.fold_left String.cat "" distance in
    int_of_string time, int_of_string distance
  | _ -> failwith "invalid input"

let rec range start end_ =
  if start > end_
  then []
  else start :: range (start + 1) end_

let beat_record (time, distance) =
  let time = float_of_int time in
  let distance = float_of_int distance in
  let r1 = time +. (sqrt (time *. time -. 4. *. distance)) /. 2. in
  let r2 = time -. (sqrt (time *. time -. 4. *. distance)) /. 2. in
  int_of_float @@ ceil r1 -. ceil r2

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
  let time, distance = parse2 lines in
  Printf.printf "Part2> %d\n" (beat_record (time, distance));
  ()

let () = main ()