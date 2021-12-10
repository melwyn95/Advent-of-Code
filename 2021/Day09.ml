Printf.printf "========== DAY 9 ==========\n" ;;

let read_file_lines filename =
  let ch = open_in filename in
  let s = String.split_on_char '\n' @@ really_input_string ch (in_channel_length ch) in
  close_in ch;
  s ;;

let zero = int_of_char '0' ;;
let input = read_file_lines "./inputs/day9" 
  |> List.map (fun s -> String.to_seq s 
    |> List.of_seq 
    |> List.map (fun c -> int_of_char c - zero) 
    |> Array.of_list)
  |> Array.of_list ;;
let r = Array.length input ;;
let c = Array.length input.(0) ;;

let within_bounds x y = x >= 0 && x < y ;;

let get_neighbours ri ci =
  let neighbours = [] in
  let neighbours = if within_bounds (ri - 1) r && within_bounds ci c then 
    (ri - 1, ci) :: neighbours else neighbours in
  let neighbours = if within_bounds (ri + 1) r && within_bounds ci c then
    (ri + 1, ci) :: neighbours else neighbours in
  let neighbours = if within_bounds ri r && within_bounds (ci - 1) c then
    (ri, ci - 1) :: neighbours else neighbours in
  if within_bounds ri r && within_bounds (ci + 1) c then
    (ri, ci + 1) :: neighbours else neighbours ;;

let part1 = Array.mapi (fun ri row -> 
  Array.mapi (fun ci cell -> 
    let neighbours = get_neighbours ri ci in
    if List.exists (fun (r, c) -> cell >= input.(r).(c)) neighbours
    then 0
    else
      cell + 1 
  ) row) input ;;

let part1 = Array.fold_left (fun s row -> Array.fold_left (+) s row) 0 part1 ;;

Printf.printf "Part 1> %d\n" part1 ;;

module SSet = Set.Make(String)
module IMap = Map.Make(Int)
  
let print_basin basin = 
  Array.iter (fun row -> 
    Array.iter (fun cell -> print_int cell; print_string " ") row;
    print_newline ()
  ) basin ;;

let basin = Array.make_matrix r c 0 
  |> Array.mapi (fun i row -> 
    Array.mapi (fun j cell ->
      if input.(i).(j) = 9 then -1 else cell
    ) row  
  ) ;;

let key i j = string_of_int i ^ "_" ^ string_of_int j ;;
let rec traverse i j n s =
  let key = key i j in
  if not (SSet.mem key !s) then
    let () = s := SSet.add key !s in
    basin.(i).(j) <- n;
    let neighbours = get_neighbours i j in
    List.iter (fun (ni, nj) ->
      if basin.(ni).(nj) = 0 
      then 
        traverse ni nj n s
    ) neighbours

let n = ref 0 ;;
let visited = ref SSet.empty ;;
Array.iteri (fun i row ->
  Array.iteri (fun j cell -> 
    (if cell = 0 
    then 
      let () = incr n in
      traverse i j !n visited
    else ())
  ) row) basin ;;

let basin_map = Array.fold_left (Array.fold_left (fun m cell ->
  IMap.update cell (function None -> Some 1 | Some n -> Some (n + 1)) m  
)) IMap.empty basin ;;

let part2 = IMap.fold (fun k v p -> 
  if k <> -1 then v :: p else p) basin_map []
  |> List.sort Int.compare
  |> List.rev
  |> function x :: y:: z :: _ -> x * y * z | _ -> failwith "error: not enough basins" ;;

Printf.printf "Part 2> %d\n" part2 ;;
