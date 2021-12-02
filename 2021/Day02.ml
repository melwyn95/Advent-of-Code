let read_file_lines filename =
  let ch = open_in filename in
  let s = String.split_on_char '\n' @@ really_input_string ch (in_channel_length ch) in
  close_in ch;
  s ;;

type command = 
    Forward of int 
  | Down    of int 
  | Up      of int ;;

let command_of_string distance = 
  function 
      "forward" -> Forward distance
    | "down"    -> Down    distance
    | "up"      -> Up      distance
    | _         -> failwith "invalid direction" ;;

let commands = read_file_lines "./inputs/day2" 
  |> List.map (fun x -> 
    let [@warning "-8"] [direction ; distance] = String.split_on_char ' ' x in
    let distance = int_of_string distance in
    command_of_string distance direction
  ) ;;

let apply_command (x, y) =
    function 
        Forward x' -> (x + x', y)
      | Down    y' -> (x, y + y')
      | Up      y' -> (x, y - y') ;;

let (x, y) = List.fold_left apply_command (0, 0) commands ;;

let part1 = x * y ;;

Printf.printf "========== DAY 2 ==========\n" ;;
Printf.printf "Part 1> %d\n" part1 ;;

let apply_command (x, y, aim) =
  function
      Forward x' -> (x + x', y + (x' * aim), aim    )
    | Down    a  -> (x     , y             , aim + a)
    | Up      a  -> (x     , y             , aim - a)

let (x, y, _) = List.fold_left apply_command (0, 0, 0) commands ;;

let part2 = x * y ;;

Printf.printf "Part 2> %d\n" part2 ;;