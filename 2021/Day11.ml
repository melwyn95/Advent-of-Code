Printf.printf "========== DAY 11 ==========\n" ;;

let read_file_lines filename =
  let ch = open_in filename in
  let s = String.split_on_char '\n' @@ really_input_string ch (in_channel_length ch) in
  close_in ch;
  s ;;

let zero  = int_of_char '0' ;;

module TSet = Set.Make (struct
  type t = (int * int)

  let compare (x1, y1) (x2, y2) =
    let xcmp = Int.compare x1 x2 in
    if xcmp = 0 then Int.compare y1 y2
    else xcmp
end)

let input = read_file_lines "./inputs/day11" 
  |> List.map String.to_seq
  |> List.map List.of_seq
  |> List.map (List.map (fun c -> int_of_char c - zero)) 
  |> List.map Array.of_list
  |> Array.of_list ;;

let n = Array.length input ;;

let print_grid xs = Array.iter (fun row -> 
  Array.iter (fun cell -> Printf.printf "%d " cell) row; print_newline ()) xs;
  print_newline () ;;

let neighbours i j n =
  [(i-1,j-1); (i-1,j); (i-1,j+1);
   (i  ,j-1);          (i  ,j+1);
   (i+1,j-1); (i+1,j); (i+1,j+1)]
   |> List.filter (fun (x, y) -> 
      x >= 0 && x < n && y >= 0 && y < n) ;;

let step' xs n =
  let fired  = ref TSet.empty in
  let incr xs = 
    Array.map (Array.map (fun n -> n + 1)) xs in
  let fire xs =
    Array.iteri (
      fun i row ->
        Array.iteri (
          fun j _ ->
            if (not (TSet.mem (i, j) !fired)) && xs.(i).(j) > 9 
            then
              let () = fired := TSet.add (i, j) !fired in
              xs.(i).(j) <- 0; 
              (neighbours i j n)
                |> List.iter (fun (x, y) ->
                  xs.(x).(y) <- xs.(x).(y) + 1
                );
            else ()
        ) row
    ) xs 
  in
  let check xs = Array.exists (Array.exists (fun cell -> cell > 9)) xs in
  let fix xs = TSet.elements !fired
    |> List.iter (fun (x, y) -> xs.(x).(y) <- 0)
  in
  let rec aux xs =
    fire xs;
    fix xs;
    if check xs then aux xs else ()
  in
  let ys = incr xs in
  aux ys;
  ys, TSet.cardinal !fired ;;

let rec step xs n m =
  if m = 1 then
    let _, flashes = step' xs n in
    flashes
  else
    let xs, flashes = step' xs n in
    flashes + step xs n (m - 1) ;;

Printf.printf "Part 1> %d\n" (step input n 100) ;;

let all_flashed xs n =
  let rec aux xs s =
    let xs, flashes = step' xs n in
    if flashes = (n * n) then s + 1
    else aux xs (s + 1)
  in
  aux xs 0 ;;

Printf.printf "Part 2> %d\n" (all_flashed input n) ;;
