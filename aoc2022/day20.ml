let read_input file =
  let chan = open_in file in
  let rec acc lines =
    try
      let line = input_line chan in
      acc (line :: lines)
    with End_of_file -> List.rev lines
  in
  let lines = acc [] in
  let () = close_in chan in
  lines

let lines = read_input "./inputs/day20.txt"

let list = List.map int_of_string lines
let size = List.length list
let n_rotate = List.map 
  (fun n -> if n < 0 then -(-n mod size) else n mod size)
  list

let rotate number xs =
  (* Printf.printf "rotate n = %d\n" number; *)
  if number = 0 then xs else
  let rec remove xs =
    match xs with
    | [] -> [], []
    | x' :: xs when number = x' -> [], xs
    | x' :: xs ->
      let xs', ys = remove xs in
      x' :: xs', ys
  in
  let rec insert_l n xs =
    if n = 0 then number :: xs
    else
      match xs with
      | [] -> failwith (Printf.sprintf "invalid list n = %d" n)
      | x :: xs ->
        x :: insert_l (n - 1) xs
  in
  let insert_r n xs =
    let xs = List.rev xs in
    let xs = insert_l n xs in
    List.rev xs
  in
  let left, right = remove xs in
  let left, right = 
    if number > 0 then
      let r_size = List.length right in
      if number >= r_size
      then 
        insert_l (number - r_size) left, right
      else 
        left, insert_l number right
    else
      let l_size = List.length left in
      if (-number) >= l_size 
      then 
        left, insert_r ((-number) - l_size) right
      else 
        insert_r (-number) left, right
  in
  left @ right

let list = List.fold_left (fun list n -> rotate n list) list n_rotate

let rec wrap_around0 xs =
  match xs with
  | x :: xs when x = 0 -> [], x :: xs
  | x :: xs -> 
    let ys, xs = wrap_around0 xs in
    x :: ys, xs
  | [] -> failwith "index out of bounds"

(* This can be avoided *)
let list = wrap_around0 list |> fun (xs, ys) -> ys @ xs

let n_1000 = 1000 mod size |> List.nth list
let n_2000 = 2000 mod size |> List.nth list
let n_3000 = 3000 mod size |> List.nth list

let () = Printf.printf "==== Day 20 ====\n"
let () = Printf.printf "Part1> %d\n" (n_1000 + n_2000 + n_3000)