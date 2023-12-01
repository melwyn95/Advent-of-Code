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

let lines = read_input "./inputs/day15.txt"

type sb = {
  sensor : int * int ;
  beacon : int * int
}

let sensor_beacon_pairs = List.map (fun line ->
  let parts = String.split_on_char ' ' line in
  let sx, sy = List.nth parts 2, List.nth parts 3 in
  let sx = List.nth (String.split_on_char '=' sx) 1 in
  let sx = List.nth (String.split_on_char ',' sx) 0 in
  let sy = List.nth (String.split_on_char '=' sy) 1 in
  let sy = List.nth (String.split_on_char ':' sy) 0 in
  let bx, by = List.nth parts 8, List.nth parts 9 in
  let bx = List.nth (String.split_on_char '=' bx) 1 in
  let bx = List.nth (String.split_on_char ',' bx) 0 in
  let by = List.nth (String.split_on_char '=' by) 1 in
  let sx, sy = int_of_string sx, int_of_string sy in
  let bx, by = int_of_string bx, int_of_string by in 
  { sensor = (sx, sy) ; beacon = (bx, by) }
) lines

let manhattan_distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

module PSet = Set.Make (struct
  type t = int * int
  let compare (x1, y1) (x2, y2) =
    match Int.compare x1 x2 with
    | 0 -> Int.compare y1 y2
    | c -> c 
end)

let find_reachable_points (x, y) dist =
  Printf.printf "%d %d dist = %d\n" x y dist;
  let rec aux (cx, cy) pset =
    if PSet.mem (cx, cy) pset || 
       manhattan_distance (x, y) (cx, cy) > dist 
    then pset
    else
      let pset = PSet.add (cx, cy) pset in
      let pset = aux (cx - 1, cy    ) pset in
      let pset = aux (cx - 1, cy - 1) pset in
      let pset = aux (cx    , cy - 1) pset in
      let pset = aux (cx + 1, cy - 1) pset in
      let pset = aux (cx + 1, cy    ) pset in
      let pset = aux (cx + 1, cy + 1) pset in
      let pset = aux (cx    , cy + 1) pset in
      let pset = aux (cx - 1, cy + 1) pset in
      pset
  in
  aux (x, y) PSet.empty

let no_beacon = sensor_beacon_pairs
|> List.fold_left (fun pset ({ sensor ; beacon }) ->
  let distance = manhattan_distance sensor beacon in
  PSet.union pset (find_reachable_points sensor distance)
) PSet.empty
|> List.fold_right (fun ({ sensor ; beacon }) pset ->
  let pset = PSet.remove beacon pset in
  let pset = PSet.remove sensor pset in
  pset
) sensor_beacon_pairs
|> PSet.filter (fun (x, y) -> y = 10)
|> PSet.cardinal

let () = Printf.printf "Part1> %d\n" no_beacon