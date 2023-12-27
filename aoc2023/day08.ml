let read_file filename =
  let ic = open_in filename in
  let rec aux () =
    try
      let line = input_line ic in
      line :: aux ()
    with _ -> []
  in
  aux ()

type direction = L | R
type path = direction list

module SMap = Map.Make (String)

type network = (string * string) SMap.t 

let parse lines =
  let path = List.hd lines in
  let path = String.fold_right (fun c ps ->
    match c with
    | 'L' -> L :: ps
    | 'R' -> R :: ps
    | _ -> failwith "invalid path char"  
  ) path [] in
  let lines = List.tl @@ List.tl lines in
  let network = List.fold_left (fun network line ->
    match String.split_on_char '=' line with
    | [ name ; neighbours ] ->
      let name = String.trim name in
      let neighbours = String.trim neighbours in
      let neighbours = 
        match String.split_on_char ',' neighbours with
        | [ left ; right ] ->
          let left = String.trim left 
            |> fun s -> String.sub s 1 (String.length s - 1)
          in
          let right = String.trim left 
            |> fun s -> String.sub s 0 (String.length s - 1)
          in
          left, right 
        | _ -> failwith "invalid neighbours"
      in
      SMap.add name neighbours network
    | _ -> failwith "invalid line"
  ) SMap.empty lines in
  path, network

let print_network =
  SMap.iter (fun name (left, right) ->
    Printf.printf "%s = %s | %s\n" name left right;  
  )

let print_path path = 
  List.iter (function L -> Printf.printf "L" | R -> Printf.printf "R") path;
  Printf.printf "\n"

let main () =
  Printf.printf "==== Day 08 ====\n";
  let input = "inputs/day08_simpl.txt" in
  (* let input = "inputs/day08.txt" in *)
  let lines = read_file input in
  let path, network = parse lines in
  print_network network;
  print_path path;
  ()

let () = main ()