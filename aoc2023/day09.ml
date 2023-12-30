let read_file filename =
  let ic = open_in filename in
  let rec aux () =
    try
      let line = input_line ic in
      line :: aux ()
    with _ -> []
  in
  aux ()

let is_all_zero = List.for_all (fun n -> n = 0)

let get_last xs = List.hd @@ List.rev xs

let extrapolate xs =
  let rec aux xs =
    match xs with
    | [ a ; b ] -> [ b - a ]
    | a :: b :: xs -> (b - a) :: aux (b :: xs)
    | _ -> failwith "toto"
  in
  let rec while_all_zero xs =
    if is_all_zero xs
    then []
    else
      let last = get_last xs in
      let xs = aux xs in
      last :: while_all_zero xs
  in
  let lasts = while_all_zero xs in
  List.fold_left ( + ) 0 lasts

let parse line =
  String.split_on_char ' ' line |> List.map int_of_string

let main () =
  Printf.printf "==== Day 09 ====\n";
  (* let input = "inputs/day09_simpl.txt" in *)
  let input = "inputs/day09.txt" in
  let lines = read_file input in
  let steps = List.map parse lines in
  let es = List.map extrapolate steps in
  (* List.iter (fun n -> Printf.printf "- %d\n" n) es; *)
  let sum_es = List.fold_left ( + ) 0 es in
  Printf.printf "Part1> %d\n" sum_es;
  ()

let () = main ()