Printf.printf "========== DAY 6 ==========\n" ;;

let read_file_lines filename =
  let ch = open_in filename in
  let s = String.split_on_char '\n' @@ really_input_string ch (in_channel_length ch) in
  close_in ch;
  s ;;

let input = read_file_lines "./inputs/day6" 
  |> List.hd 
  |> String.split_on_char ',' 
  |> List.map int_of_string ;;

let initial_state, fish = (input, ref (List.length input)) ;;

let state = Array.make 9 0 ;;

List.iter (fun s -> state.(s) <- state.(s) + 1) initial_state ;;

let step state fish =
  let z = state.(0) in
  for i = 1 to 8 do
    state.(i - 1) <- state.(i)
  done;
  state.(6) <- state.(6) + z;
  state.(8) <- z;
  !fish + z ;;
  
for n = 1 to 80 do
  fish := step state fish
done ;;

Printf.printf "Part 1> %d\n" !fish ;;

for n = 1 to (256 - 80) do
  fish := step state fish
done;;

Printf.printf "Part 2> %d\n" !fish ;;