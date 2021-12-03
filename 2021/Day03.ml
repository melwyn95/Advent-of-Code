let read_file_lines filename =
  let ch = open_in filename in
  let s = String.split_on_char '\n' @@ really_input_string ch (in_channel_length ch) in
  close_in ch;
  s ;;

type bit = O | I ;;

type bin = bit list ;;

let bin_of_string s = 
  let rec aux i length bits =
    if i = length then bits else
    match String.get s i with
      '0' -> aux (i + 1) length (O :: bits)
    | '1' -> aux (i + 1) length (I :: bits)
    | _   -> failwith ("invalid binary: " ^ s)
  in
  List.rev @@ aux 0 (String.length s) [] ;;

let int_of_bin bits =
  let bits = List.rev bits in
  snd @@ List.fold_left (fun (p,t) b ->
      match b with
        O -> (p * 2, t)
      | I -> (p * 2, t + p)
    ) 
    (1, 0) bits ;;

let invert_bin bits =
  List.map (fun b -> if b = I then O else I) bits

let report = read_file_lines "./inputs/day3" |> List.map bin_of_string ;;

let most_common_bin bins =
  let count = List.map (fun _ -> 0) @@ List.hd bins in
  let count = List.fold_left 
    (fun count bin ->
      List.map2 
        (fun c b -> 
          match c, b with
            c, O -> c - 1
          | c, I -> c + 1) 
        count bin  
    ) 
    count bins in
  List.map (fun c -> if c < 0 then O else I) count ;;

let mcb = most_common_bin report ;;
let lcb = invert_bin mcb ;;

let gamma_rate = int_of_bin mcb ;;
let epsilon_rate = int_of_bin lcb ;;

let part1 = gamma_rate * epsilon_rate ;;

Printf.printf "========== DAY 3 ==========\n" ;;
Printf.printf "Part 1> %d\n" part1 ;;

let generator_rating f report =
  let report = List.map (fun bin -> (int_of_bin bin, bin)) report in
  let rec aux report = 
    if List.length report = 1 then report else
    let bits = List.map (fun (_,b) -> List.hd b) report in
    let (ones, zeros) = List.fold_left 
      (fun (o,z) b -> if b = I then (o + 1, z) else (o, z + 1)) 
      (0,0) bits in
    let bit = f ones zeros in
    let report = List.filter (fun (_, b) -> bit = List.hd b) report in
    let report = List.map (fun (d, b) -> (d, List.tl b)) report in
    aux report
  in
  fst @@ List.hd @@ aux report ;;

let o2_generator_rating = generator_rating 
  (fun o z -> if o = z then I else if o > z then I else O) 
  report ;;

let co2_scrubber_rating = generator_rating
  (fun o z -> if o = z then O else if o < z then I else O)
  report ;;

let part2 = o2_generator_rating * co2_scrubber_rating ;;

Printf.printf "Part 2> %d\n" part2 ;;