Printf.printf "========== DAY 10 ==========\n" ;;

let read_file_lines filename =
  let ch = open_in filename in
  let s = String.split_on_char '\n' @@ really_input_string ch (in_channel_length ch) in
  close_in ch;
  s ;;

type token = 
    Open_Square 
  | Close_Square
  | Open_Round
  | Close_Round
  | Open_Curly
  | Close_Curly
  | Open_Angle
  | Close_Angle

let print_token = 
  function
    Open_Square  -> print_string "[" 
  | Close_Square -> print_string "]"
  | Open_Round   -> print_string "("
  | Close_Round  -> print_string ")"
  | Open_Curly   -> print_string "{"
  | Close_Curly  -> print_string "}"
  | Open_Angle   -> print_string "<"
  | Close_Angle  -> print_string ">"

type status = Complete | InComplete of token list | Corrupted of token

let parse_line line = 
  String.to_seq line 
    |> List.of_seq
    |> List.map 
      (function 
          '[' -> Open_Square
        | ']' -> Close_Square
        | '(' -> Open_Round
        | ')' -> Close_Round
        | '{' -> Open_Curly
        | '}' -> Close_Curly
        | '<' -> Open_Angle
        | '>' -> Close_Angle
        | _   -> failwith "invalid token") ;;

let input = read_file_lines "./inputs/day10" |> List.map parse_line ;;

module Stack = struct
  type 'a t = 'a list

  let push x xs = x :: xs

  let pop = function [] -> None | x :: xs -> (Some (x, xs))

  let is_empty xs = xs = []

end

let complement = 
  function
    Open_Square -> Close_Square
  | Open_Round  -> Close_Round
  | Open_Curly  -> Close_Curly
  | Open_Angle  -> Close_Angle
  | _           -> failwith "error complement" ;;

let unwind_stack stack =
  let rec aux stack completion =
    match stack with
      [] -> List.rev completion
    | token :: stack -> aux stack (complement token :: completion)
  in
  aux stack [] ;;

let rec process_tokens tokens stack = 
  match tokens with
      []              -> if Stack.is_empty stack then Complete else InComplete (unwind_stack stack)
    | (Open_Square as token) :: tokens
    | (Open_Round  as token) :: tokens
    | (Open_Curly  as token) :: tokens
    | (Open_Angle  as token) :: tokens ->
      process_tokens tokens (Stack.push token stack)
    | (Close_Square as token) :: tokens ->
      (match Stack.pop stack with
        Some (Open_Square, stack) -> process_tokens tokens stack
      | Some (_, stack)       -> if tokens = [] then InComplete (unwind_stack stack) else Corrupted token
      | None                      -> Corrupted Close_Square)
    | (Close_Round as token) :: tokens ->
      (match Stack.pop stack with
        Some (Open_Round, stack) -> process_tokens tokens stack
      | Some (t, stack)      -> if tokens = [] then InComplete (unwind_stack stack) else Corrupted token
      | None                     -> Corrupted Close_Round)
    | (Close_Curly as token) :: tokens ->
      (match Stack.pop stack with
        Some (Open_Curly, stack) -> process_tokens tokens stack
      | Some (t, stack)      -> if tokens = [] then InComplete (unwind_stack stack) else Corrupted token
      | None                     -> Corrupted Close_Curly)
    | (Close_Angle as token) :: tokens ->
      (match Stack.pop stack with
        Some (Open_Angle, stack) -> process_tokens tokens stack
      | Some (t, stack)      -> if tokens = [] then InComplete (unwind_stack stack) else Corrupted token
      | None                     -> Corrupted Close_Angle) ;;

let score = function 
    Close_Square -> 57 
  | Close_Round  -> 3 
  | Close_Curly  -> 1197 
  | Close_Angle  -> 25137
  | token        -> failwith "invalid corruped token: " ;;

let part1 = List.filter_map (fun tokens -> 
  match process_tokens tokens [] with
    Corrupted token -> Some (score token)
  | _               -> None) input 
  |> List.fold_left (+) 0 ;;

Printf.printf "Part 1> %d\n" part1 ;;

let score = function 
    Close_Square -> 2 
  | Close_Round  -> 1
  | Close_Curly  -> 3 
  | Close_Angle  -> 4
  | token        -> failwith "invalid completion token: " ;;

let pluck_middle xs =
  let length = List.length xs in
  let rec aux xs length =
    match xs with
    | x :: xs when length = 1 -> x
    | x :: xs -> aux xs (length - 2)
    | _ -> failwith "impossible" 
  in
  aux xs length ;;

let part2 = List.filter_map (fun tokens -> 
  match process_tokens tokens [] with
    InComplete completion -> Some completion
  | _          -> None) input 
  |> List.map (fun ts -> List.fold_left (fun s t -> score t + 5 * s) 0 ts) 
  |> List.sort Int.compare
  |> pluck_middle ;;

Printf.printf "Part 2> %d\n" part2 ;;
