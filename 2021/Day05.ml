(* ocamlc str.cma Day05.ml *)
Printf.printf "========== DAY 4 ==========\n" ;;

let read_file_lines filename =
  let ch = open_in filename in
  let s = String.split_on_char '\n' @@ really_input_string ch (in_channel_length ch) in
  close_in ch;
  s ;;

let input = read_file_lines "./inputs/day5" 
  |> List.map (fun s -> Str.replace_first (Str.regexp " -> ") " " s)
  |> List.map (fun s -> 
    match String.split_on_char ' ' s with
      a :: b :: [] -> (a, b)
    | _ -> failwith "error parsing points")
  |> List.map (fun (a, b) -> 
    match (String.split_on_char ',' a), (String.split_on_char ',' b) with
      x1 :: y1 :: [], x2 :: y2 :: [] -> 
        (int_of_string x1, int_of_string y1), (int_of_string x2, int_of_string y2)
    | _ -> failwith "error parsing point") ;;

let horizontal ((_, y1), (_, y2)) = y1 = y2

let vertical ((x1, _), (x2, _)) = x1 = x2

let diagonal ((x1, y1), (x2, y2)) = x1 = y1 && y1 == y2

let lines,diagonals = input |> List.partition (fun l -> horizontal l || vertical l && not @@ diagonal l) 

let grid = Array.make_matrix 1000 1000 0 ;;

List.iter (fun ((x1,y1), (x2,y2)) -> 
  if x1 = x2 
  then
    for y = min y1 y2 to max y1 y2 do
      grid.(x1).(y) <- grid.(x1).(y) + 1 
    done
  else if y1 = y2
  then
    for x = min x1 x2 to max x1 x2 do
      grid.(x).(y1) <- grid.(x).(y1) + 1 
    done
  else ()
) lines ;;
let overlaps = ref 0 ;;

for x = 0 to 999 do
  for y = 0 to 999 do
    if grid.(x).(y) > 1 then incr overlaps
  done
done ;;

Printf.printf "Part 1> %d\n" !overlaps ;;

List.iter (fun ((x1,y1), (x2,y2)) -> 
  let times = abs (x1 - x2) in
  let xd, yd = 
    if x1 > x2 && y1 > y2 then false, false else
    if x1 > x2 && y1 < y2 then false, true else
    if x1 < x2 && y1 > y2 then true, false else
    if x1 < x2 && y1 < y2 then true, true else
    failwith "boom" in
  for t = 0 to times do
    match xd, yd with
      true, true   -> grid.(x1 + t).(y1 + t) <- grid.(x1 + t).(y1 + t) + 1 
    | true, false  -> grid.(x1 + t).(y1 - t) <- grid.(x1 + t).(y1 - t) + 1
    | false, true  -> grid.(x1 - t).(y1 + t) <- grid.(x1 - t).(y1 + t) + 1
    | false, false -> grid.(x1 - t).(y1 - t) <- grid.(x1 - t).(y1 - t) + 1
  done
) diagonals ;;

let overlaps = ref 0 ;;

for x = 0 to 999 do
  for y = 0 to 999 do
    if grid.(x).(y) > 1 then incr overlaps
  done
done ;;

Printf.printf "Part 2> %d\n" !overlaps ;;
