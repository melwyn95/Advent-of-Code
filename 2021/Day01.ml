Printf.printf "========== DAY 1 ==========\n" ;;

let read_file_lines filename =
  let ch = open_in filename in
  let s = String.split_on_char '\n' @@ really_input_string ch (in_channel_length ch) in
  close_in ch;
  s ;;

let input = read_file_lines "./inputs/day1" |> List.map int_of_string ;;

let diff xs = snd @@ List.fold_left 
  (fun (prev, inc) curr ->
    match prev with
      Some prev -> Some curr, (if curr - prev > 0 then inc + 1 else inc)
    | None -> Some curr, inc) 
  (None, 0) xs ;;

let part1 = diff input ;;

Printf.printf "Part 1> %d\n" part1 ;;

let triplets xs =
  let rec aux xs ys =
    match xs with
      x :: y :: z :: [] -> (x,y,z) :: ys
    | x :: y :: z :: xs -> aux (y :: z :: xs) ((x,y,z)::ys)
    | _                 -> ys
  in
  List.rev @@ aux xs []

let part2 = triplets input 
  |> List.map (fun (x,y,z) -> x + y + z)
  |> diff ;;

Printf.printf "Part 2> %d\n" part2 ;;