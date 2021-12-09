Printf.printf "========== DAY 8 ==========\n" ;;

let read_file_lines filename =
  let ch = open_in filename in
  let s = String.split_on_char '\n' @@ really_input_string ch (in_channel_length ch) in
  close_in ch;
  s ;;

let sort_string s =
  let a, b, c, d, e, f, g 
    = ref false, ref false, ref false, ref false, ref false, ref false, ref false in
  String.iter (function 
      'a' -> a := true
    | 'b' -> b := true  
    | 'c' -> c := true
    | 'd' -> d := true
    | 'e' -> e := true
    | 'f' -> f := true
    | 'g' -> g := true
    | _   -> failwith "invalid char") s;
  (if !a then "a" else "") ^
  (if !b then "b" else "") ^
  (if !c then "c" else "") ^
  (if !d then "d" else "") ^
  (if !e then "e" else "") ^
  (if !f then "f" else "") ^
  (if !g then "g" else "")
  ;;

let input = read_file_lines "./inputs/day8" 
  |> List.map @@ String.split_on_char '|'
  |> List.map @@ (function [a; b] -> (a, b) | _ -> failwith "invalid input")
  |> List.map (fun (a, b) -> (String.split_on_char ' ' a, String.split_on_char ' ' b))
  |> List.map (fun (a, b) -> (List.filter (fun s -> s <> "") a, List.filter (fun s -> s <> "") b)) ;;

let sum = List.fold_left (+) 0

let ones = sum @@ List.map (fun (a, b) ->
  let one = sort_string @@ List.hd @@ List.filter (fun s -> String.length s = 2) a in
  List.length @@ List.filter (fun s -> (sort_string s) =one) b
) input ;;

let fours = sum @@ List.map (fun (a, b) ->
  let four = sort_string @@ List.hd @@ List.filter (fun s -> String.length s = 4) a in  
  List.length @@ List.filter (fun s -> (sort_string s) =four) b
) input ;;

let sevens = sum @@ List.map (fun (a, b) ->
  let seven = sort_string @@ List.hd @@ List.filter (fun s -> String.length s = 3) a in  
  List.length @@ List.filter (fun s -> (sort_string s) =seven) b
) input ;;

let eights = sum @@ List.map (fun (a, b) ->
  let eight = sort_string @@ List.hd @@ List.filter (fun s -> String.length s = 7) a in  
  List.length @@ List.filter (fun s -> (sort_string s) =eight) b
) input ;;

let part1 = ones + fours + sevens + eights ;;

Printf.printf "Part 1> %d\n" part1 ;;

module M = Map.Make(Int)
module S = Set.Make(Char)
module CMap = Map.Make(Char)

let get_mapping xs =
  let xs   = List.map (fun x -> String.to_seq x |> S.of_seq) xs in
  let xs   = List.fold_left (fun m x -> M.update (S.cardinal x) (function None -> Some [x] | Some xs -> Some (x::xs)) m) M.empty xs in
  let _1   = List.hd @@ M.find 2 xs in
  let _4   = List.hd @@ M.find 4 xs in
  let _7   = List.hd @@ M.find 3 xs in
  let _8   = List.hd @@ M.find 7 xs in
  let abfg = M.find 6 xs |> List.fold_left S.inter _8 in
  let adg  = M.find 5 xs |> List.fold_left S.inter _8 in
  let a    = S.diff _7 _1 |> S.elements |> List.hd in
  let cf   = S.inter _1 _4 in
  let bd   = S.diff _4 _1 in
  let eg   = S.diff (S.diff _8 _7) _4 in
  let dg   = S.remove a adg in
  let g    = S.inter dg eg |> S.elements |> List.hd in
  let d    = S.remove g dg |> S.elements |> List.hd in
  let e    = S.remove g eg |> S.elements |> List.hd in
  let b    = S.remove d bd |> S.elements |> List.hd in
  let f    = abfg |> S.remove a |> S.remove b |> S.remove g |> S.elements |> List.hd in
  let c    = S.remove f cf |> S.elements |> List.hd in
  CMap.empty
    |> CMap.add a 'a' 
    |> CMap.add b 'b'
    |> CMap.add c 'c'
    |> CMap.add d 'd'
    |> CMap.add e 'e'
    |> CMap.add f 'f'
    |> CMap.add g 'g' ;;

let substitute s mapping = String.map (fun c -> CMap.find c mapping) s |> sort_string ;;

let get_number = 
  function 
      "abcefg"  -> "0" 
    | "cf"      -> "1"
    | "acdeg"   -> "2"
    | "acdfg"   -> "3"
    | "bcdf"    -> "4"
    | "abdfg"   -> "5"
    | "abdefg"  -> "6"
    | "acf"     -> "7"
    | "abcdefg" -> "8"
    | "abcdfg"  -> "9"
    | s -> failwith ("invalid arrangement '" ^ s ^ "'") ;;

let find_number ss xs  =
  let mapping = get_mapping xs in
  (List.map (fun s -> 
    get_number (substitute s mapping)   
  ) ss) 
    |> String.concat ""
    |> int_of_string ;;


let part2 = List.fold_left (fun s (xs, ss) -> s + find_number ss xs) 0 input ;;

Printf.printf "Part 2> %d\n" part2 ;;
