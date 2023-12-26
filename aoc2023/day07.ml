let read_file filename =
  let ic = open_in filename in
  let rec aux () =
    try
      let line = input_line ic in
      line :: aux ()
    with _ -> []
  in
  aux ()

type card = A | K | Q | J | T | Num of int

let card_score c =
  match c with
  | A -> 13
  | K -> 12
  | Q -> 11
  | J -> 10
  | T -> 9
  | Num 9 -> 8
  | Num 8 -> 7
  | Num 7 -> 6
  | Num 6 -> 5
  | Num 5 -> 4
  | Num 4 -> 3
  | Num 3 -> 2
  | Num 2 -> 1
  | _ -> failwith "invalid card"

let compare_cards c1 c2 =
  Int.compare (card_score c1) (card_score c2)

let compare_hand h1 h2 =
  let c11, c12, c13, c14, c15 = h1 in
  let c21, c22, c23, c24, c25 = h2 in
  match compare_cards c11 c21 with
  | 0 -> (match compare_cards c12 c22 with
          | 0 -> (match compare_cards c13 c23 with
                  | 0 -> (match compare_cards c14 c24 with
                          | 0 -> compare_cards c15 c25
                          | n -> n)
                  | n -> n)
          | n -> n)
  | n -> n

let print_card = function
  | A -> Printf.printf "A"
  | K -> Printf.printf "K"
  | Q -> Printf.printf "Q"
  | J -> Printf.printf "J"
  | T -> Printf.printf "T"
  | Num n -> Printf.printf "%d" n

let print_hand (c1, c2, c3, c4, c5) =
  print_card c1;
  print_card c2;
  print_card c3;
  print_card c4;
  print_card c5;
  Printf.printf "\n";

module CardSet = Set.Make (struct 
  type t = card
  let compare = compare_cards
end)

let card_of_string = function
  | "A" -> A
  | "K" -> K
  | "Q" -> Q
  | "J" -> J
  | "T" -> T
  | ("2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") as n 
    -> Num (int_of_string n)
  | _ -> failwith "invalid card"

type hand = card * card * card * card * card 

type row = hand * int

let parse line =
  match String.split_on_char ' ' line with
  | [ hand ; bid ] -> 
    let hand = 
      match List.of_seq @@ String.to_seq hand with
      | [ c1 ; c2 ; c3 ; c4 ; c5 ] -> 
        card_of_string (String.make 1 c1),
        card_of_string (String.make 1 c2),
        card_of_string (String.make 1 c3),
        card_of_string (String.make 1 c4),
        card_of_string (String.make 1 c5)
      | _ -> failwith "invalid hand"
    in
    hand, int_of_string bid
  | _ -> failwith "invalid row"

type hand_type =
    Five_of_a_kind of card
  | Four_of_a_kind of card * card
  | Full_house of card * card
  | Three_of_a_kind of card * card * card
  | Two_pair of card * card * card
  | One_pair of card * card * card * card
  | High_card of card * card * card * card * card

let compare_hand_type h1 h2 =
  match h1, h2 with
  | Five_of_a_kind _, Five_of_a_kind _ -> 0
  | Five_of_a_kind _, _ -> 1
  | Four_of_a_kind _, Five_of_a_kind _ -> -1
  | Four_of_a_kind _, Four_of_a_kind _ -> 0
  | Four_of_a_kind _, _ -> 1
  | Full_house _, (Five_of_a_kind _ | Four_of_a_kind _) -> -1
  | Full_house _, Full_house _ -> 0
  | Full_house _, _ -> 1
  | Three_of_a_kind _, (Five_of_a_kind _ | Four_of_a_kind _ | Full_house _) -> -1
  | Three_of_a_kind _, Three_of_a_kind _ -> 0
  | Three_of_a_kind _, _ -> 1
  | Two_pair _, (Five_of_a_kind _ | Four_of_a_kind _ | Full_house _ | Three_of_a_kind _) -> -1
  | Two_pair _, Two_pair _ -> 0
  | Two_pair _, _ -> 1
  | One_pair _, 
    (Five_of_a_kind _ | Four_of_a_kind _ | Full_house _ | Three_of_a_kind _ | Two_pair _) -> -1
  | One_pair _, One_pair _ -> 0
  | One_pair _, _ -> 1
  | High_card _, High_card _ -> 0
  | High_card _, _ -> -1

let find_card_count card hand =
  let c1, c2, c3, c4, c5 = hand in
  let count = 0 in
  let count = count + if c1 = card then 1 else 0 in
  let count = count + if c2 = card then 1 else 0 in
  let count = count + if c3 = card then 1 else 0 in
  let count = count + if c4 = card then 1 else 0 in
  let count = count + if c5 = card then 1 else 0 in
  count

let find_four_of_a_find card_set hand =
  match List.of_seq @@ CardSet.to_seq card_set with
  | [ c1 ; c2 ] ->
    if find_card_count c1 hand = 4 then Some (c1, c2) 
    else if find_card_count c2 hand = 4 then Some (c2, c1)
    else None
  | _ -> failwith "not four of a kind"

let find_full_house card_set hand =
  match List.of_seq @@ CardSet.to_seq card_set with
  | [ c1 ; c2 ] ->
    if find_card_count c1 hand = 3 then c1, c2
    else if find_card_count c2 hand = 3 then c2, c1
    else failwith "incorrect full house"
  | _ -> failwith "not full house"

let find_one_pair card_set hand =
  match List.of_seq @@ CardSet.to_seq card_set with
  | [ c1 ; c2 ; c3 ; c4 ] ->
    if find_card_count c1 hand = 2 then c1, c2, c3, c4 
    else if find_card_count c2 hand = 2 then c2, c1, c3, c4
    else if find_card_count c3 hand = 2 then c3, c1, c2, c4
    else c4, c1, c2, c3
  | _ -> failwith "not one pair"

let find_three_of_a_kind card_set hand =
  match List.of_seq @@ CardSet.to_seq card_set with
  | [ c1 ; c2 ; c3 ] ->
    if find_card_count c1 hand = 3 then Some (c1, c2, c3)
    else if find_card_count c2 hand = 3 then Some (c2, c1, c3)
    else if find_card_count c3 hand = 3 then Some (c3, c1, c2)
    else None
  | _ -> failwith "not three of a kind"

let find_two_pair card_set hand =
  match List.of_seq @@ CardSet.to_seq card_set with
  | [ c1 ; c2 ; c3 ] ->
    if find_card_count c1 hand = 1 then c2, c3, c1
    else if find_card_count c2 hand = 1 then c1, c3, c2
    else c1, c2, c3
  | _ -> failwith "not two pair"

let hand_type : hand -> hand_type =
  fun hand ->
    let c1, c2, c3, c4, c5 = hand in 
    let cards = CardSet.add_seq (List.to_seq [c1;c2;c3;c4;c5]) CardSet.empty in 
    match CardSet.cardinal cards with
    | 5 -> High_card (c1, c2, c3, c4, c5)
    | 4 -> 
      let c1, c2, c3, c4 = find_one_pair cards hand in
      One_pair (c1, c2, c3, c4)
    | 3 ->
      (match find_three_of_a_kind cards hand with
      | Some (c1, c2, c3) -> Three_of_a_kind (c1, c2, c3)
      | None -> 
        let c1, c2, c3 = find_two_pair cards hand in
        Two_pair (c1, c2, c3))
    | 2 -> 
      (match find_four_of_a_find cards hand with
      | Some (c1, c2) -> Four_of_a_kind (c1, c2)
      | None ->
        let c1, c2 = find_full_house cards hand in
        Full_house (c1, c2))
    | 1 -> Five_of_a_kind c1
    | _ -> failwith "invalid hand"

let main () =
  Printf.printf "==== Day 07 ====\n";
  (* let input = "inputs/day07_simpl.txt" in *)
  let input = "inputs/day07.txt" in
  let lines = read_file input in
  let rows = List.map parse lines in
  let rows = List.map (fun (hand, rank) ->hand, hand_type hand, rank) rows in
  let rows = List.sort (fun (hand1, hand_type1, _) (hand2, hand_type2, _) -> 
    match compare_hand_type hand_type1 hand_type2 with
    | 0 -> compare_hand hand1 hand2
    | n -> n
  ) rows in
  let rows = List.mapi (fun i (_, _, bid) -> (i + 1) * bid) rows in
  let total_winnings = List.fold_left ( + ) 0 rows in
  Printf.printf "Part1> %d\n" total_winnings;
  ()

let () = main ()