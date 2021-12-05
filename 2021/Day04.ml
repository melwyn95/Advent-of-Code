Printf.printf "========== DAY 4 ==========\n" ;;

let read_file_lines filename =
  let ch = open_in filename in
  let s = String.split_on_char '\n' @@ really_input_string ch (in_channel_length ch) in
  close_in ch;
  s ;;

let input = read_file_lines "./inputs/day4" ;;

let draw_numbers = List.hd input 
  |> String.split_on_char ',' 
  |> List.map int_of_string ;;

let input = List.tl @@ List.tl input

module Board = struct
  type cell = 
      Marked of int
    | UnMarked of int 
  type t = cell array array

  let make a b c d e : t =
    let f = List.filter (fun s -> s <> "") in
    let a = f @@ String.split_on_char ' ' a in
    let b = f @@ String.split_on_char ' ' b in
    let c = f @@ String.split_on_char ' ' c in
    let d = f @@ String.split_on_char ' ' d in
    let e = f @@ String.split_on_char ' ' e in
    let board = Array.make_matrix 5 5 (UnMarked 0) in
    let () = List.iteri (fun i ai -> board.(0).(i) <- UnMarked (int_of_string ai)) a in
    let () = List.iteri (fun i bi -> board.(1).(i) <- UnMarked (int_of_string bi)) b in
    let () = List.iteri (fun i ci -> board.(2).(i) <- UnMarked (int_of_string ci)) c in
    let () = List.iteri (fun i di -> board.(3).(i) <- UnMarked (int_of_string di)) d in
    let () = List.iteri (fun i ei -> board.(4).(i) <- UnMarked (int_of_string ei)) e in
    board ;;

  let parse input = 
    match input with
      a :: b :: c :: d :: e :: _ :: next -> Some (make a b c d e, next)
    | a :: b :: c :: d :: e :: next      -> Some (make a b c d e, next)
    | _ -> None

  let print board =
    print_endline "===================="; 
    Array.iter (
      fun row ->
        let s = Array.fold_left (fun s cell -> 
          match cell with
            UnMarked cell -> s ^ " " ^ (string_of_int cell)
          | Marked   cell -> s ^ " " ^ "X") "" row in
        print_endline s
    ) board;
    print_endline "===================="

  let mark n (board : t) : t =
    Array.map (
      fun row ->
        Array.map (
          fun cell ->
            match cell with
              UnMarked c -> if c = n then Marked c else UnMarked c
            | Marked   c -> Marked c
        ) row
    ) board

  let check_row i board =
    let row = board.(i) in
    match row.(0), row.(1), row.(2), row.(3), row.(4) with
      Marked _, Marked _, Marked _, Marked _, Marked _ -> true
    | _ -> false

  let check_col i board =
    match board.(0).(i), board.(1).(i), board.(2).(i), board.(3).(i), board.(4).(i) with
      Marked _, Marked _, Marked _, Marked _, Marked _ -> true
    | _ -> false

  let check (board : t) : bool =
     check_row 0 board ||
     check_row 1 board ||
     check_row 2 board ||
     check_row 3 board ||
     check_row 4 board ||
     check_col 0 board ||
     check_col 1 board ||
     check_col 2 board ||
     check_col 3 board ||
     check_col 4 board

  let unmarked_sum board : int =
    Array.fold_left (fun s row -> 
      Array.fold_left (fun s cell ->
        match cell with
          UnMarked c -> s + c
        | Marked   _ -> s   
      ) s row  
    ) 0 board

end ;;

let rec parse_boards input =
  let p = Board.parse input in
  match p with
    Some (b, input) -> b :: parse_boards input
  | None -> [] ;;

let boards = parse_boards input ;;

let play draw_numbers boards =
  List.fold_left (fun (status, boards) draw ->
    match status with
      Some s -> (Some s, boards)
    | None -> 
      let boards = List.map (Board.mark draw) boards in
      (List.fold_left (fun status board ->
        match status with
          Some board -> Some board
        | None ->
          if Board.check board 
          then Some (board, draw) 
          else None  
      ) None boards, boards)
  ) (None, boards) draw_numbers ;;

let board_opt, boards = play draw_numbers boards ;;

let part1 = 
  match board_opt with
    Some (board, draw) -> draw * Board.unmarked_sum board
  | None -> failwith "No solution: part1" ;;

Printf.printf "Part 1> %d\n" part1 ;;
  
let play draw_numbers boards =
  let rec aux draw_numbers boards = 
    match draw_numbers with
      [] -> failwith "all draws done"
    | draw :: draw_numbers ->
      let boards = List.map (Board.mark draw) boards in
      let boards' = List.filter (fun board -> not @@ Board.check board) boards in
      if List.length boards' = 0 then List.hd boards, draw
      else  aux draw_numbers boards'
  in
  aux draw_numbers boards ;;

let board, draw = play draw_numbers boards ;;

let part2 = draw * Board.unmarked_sum board ;;

Printf.printf "Part 2> %d\n" part2 ;;