Printf.printf "========== DAY 7 ==========\n" ;;

let read_file_lines filename =
  let ch = open_in filename in
  let s = String.split_on_char '\n' @@ really_input_string ch (in_channel_length ch) in
  close_in ch;
  s ;;

let input = read_file_lines "./inputs/day7" 
  |> List.hd 
  |> String.split_on_char ',' 
  |> List.map int_of_string ;;

let s, e = List.fold_left (fun (s, e) i -> (min s i), (max e i)) (max_int, min_int) input ;;

let minimum_fuel xs =
  let rec aux x =
    List.fold_left (fun s x' -> s + abs (x - x')) 0 xs
  in
  let rec foo ys fuel =
    match ys with
      [] -> fuel
    | y :: ys -> foo ys (min fuel (aux y))
  in
  foo xs (max_int) ;;

Printf.printf "Part 1> %d\n" (minimum_fuel input) ;;

let minimum_fuel s e xs =
  let cum_sum n = (n * (n + 1)) / 2 in
  let rec aux x =
    List.fold_left (fun s x' -> s + cum_sum (abs (x - x'))) 0 xs
  in
  let rec foo s e fuel =
    if s > e then fuel
    else foo (s + 1) e (min fuel (aux s))
  in
  foo s e (max_int) ;;

Printf.printf "Part 2> %d\n" (minimum_fuel s e input) ;;