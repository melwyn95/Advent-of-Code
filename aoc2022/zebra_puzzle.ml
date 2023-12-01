type nationality = 
    English
  | Spainiard
  | Ukrainian
  | Japanese
  | Norwegian
    
module NSet = Set.Make(struct 
  type t = nationality
  let compare = Stdlib.compare
end)

let print_nationality = function
    English   -> "English"
  | Spainiard -> "Spainiard"
  | Ukrainian -> "Ukrainian"
  | Japanese  -> "Japanese"
  | Norwegian -> "Norwegian"

type smoke =
    Kools
  | OldGold
  | Chesterfield
  | LuckyStrike
  | Parliament

module SSet = Set.Make(struct 
  type t = smoke
  let compare = Stdlib.compare
end)

let print_smoke = function  
    Kools -> "Kools"
  | OldGold -> "OldGold"
  | Chesterfield -> "Chesterfield"
  | LuckyStrike -> "LuckyStrike"
  | Parliament -> "Parliament"

type drink =
    Milk
  | Water
  | Coffee
  | Tea
  | OrangeJuice

module DSet = Set.Make(struct 
  type t = drink
  let compare = Stdlib.compare
end)

let print_drink = function
    Milk -> "Milk"
  | Water -> "Water"
  | Coffee -> "Coffee"
  | Tea -> "Tea"
  | OrangeJuice -> "OrangeJuice"

type owns =
    Dog
  | Horse
  | Fox
  | Zebra
  | Snail

module OSet = Set.Make(struct 
  type t = owns
  let compare = Stdlib.compare
end)

let print_owns = function
    Dog -> "Dog"
  | Horse -> "Horse"
  | Fox -> "Fox"
  | Zebra -> "Zebra"
  | Snail -> "Snail"    

type color =
    Red
  | Blue
  | Yellow
  | Ivory
  | Green
  
module CSet = Set.Make(struct 
  type t = color
  let compare = Stdlib.compare
end)
    
let print_color = function
    Red -> "Red"
  | Blue -> "Blue"
  | Yellow -> "Yellow"
  | Ivory -> "Ivory"
  | Green -> "Green"


type house = nationality * smoke * drink * owns * color    

let print_house (n, s, d, o, c) =
  Printf.sprintf "Nationality = %s, Smoke = %s, Drink = %s, Owns = %s, Color = %s" 
    (print_nationality n)
    (print_smoke s)
    (print_drink d)
    (print_owns o)
    (print_color c)

type street = house * house * house * house * house

let print_street (h1, h2, h3, h4, h5) =
  Printf.sprintf "House1 = %s\nHouse2 = %s\nHouse3 = %s\nHouse4 = %s\nHouse5 = %s\n"
    (print_house h1)
    (print_house h2)
    (print_house h3)
    (print_house h4)
    (print_house h5)

let next_to f (h1, h2, h3, h4, h5) =
  f h1 h2
  || f h2 h1 || f h2 h3
  || f h3 h2 || f h3 h4
  || f h4 h3 || f h4 h5
  || f h5 h4
  
let right_of f (h1, h2, h3, h4, h5) = 
  f h2 h1
  || f h3 h2
  || f h4 h3
  || f h5 h4
    
let any f (h1, h2, h3, h4, h5) =
  f h1 || f h2 || f h3 || f h4 || f h5

(* let fact_2 = any @@
  function 
    (English, _, _, _, Red) -> true 
  | _ -> false
let fact_3 = any @@
  function 
    (Spainiard, _, _, Dog, _) -> true
  | _ -> false
let fact_4 = any @@
  function 
    (_, _, Coffee, _, Green) -> true
  | _ -> false
let fact_5 = any @@
  function 
    (Ukrainian, _, Tea, _, _) -> true 
  | _ -> false *)
let fact_6 = right_of @@
  fun h1 h2 ->
  match h1, h2 with
    (_, _, _, _, Green), (_, _, _, _, Ivory) -> true
  | _ -> false
(* let fact_7 = any @@
  function
    (_, OldGold, _, Snail, _) -> true
  | _ -> false
let fact_8 = any @@
  function
    (_, Kools, _, _, Yellow) -> true
  | _ -> false  *)
(* let fact_9 (_, _, h, _, _) = 
  match h with
    (_, _, Milk, _, _) -> true
  | _ -> false
let fact_10 (h, _, _, _, _) =
  match h with
    (Norwegian, _, _, _, _) -> true
  | _ -> false *)
let fact_11 = next_to @@
  fun h1 h2 ->
  match h1, h2 with
    (_, Chesterfield, _, _, _), (_, _, _, Fox, _) -> true
  | _ -> false
let fact_12 = next_to @@
  fun h1 h2 ->
  match h1, h2 with
    (_, Kools, _, _, _), (_, _, _, Horse, _) -> true
  | _ -> false
(* let fact_13 = any @@
  function
    (_, LuckyStrike, OrangeJuice, _, _) -> true
  | _ -> false
let fact_14 = any @@
  function
    (Japanese, Parliament, _, _, _) -> true
  | _ -> false *)
let fact_15 = next_to @@
  fun h1 h2 ->
  match h1, h2 with
    (Norwegian, _, _, _, _), (_, _, _, _, Blue) -> true
  | _ -> false
  
  
let is_house_valid h = 
  match h with
    (English, _, _, _, Red) -> true 
  | (English, _, _, _, _) -> false
  | (_, _, _, _, Red) -> false
  
  | (Spainiard, _, _, Dog, _) -> true
  | (_, _, _, Dog, _) -> false
  | (Spainiard, _, _, _, _) -> false
  
  | (_, _, Coffee, _, Green) -> true
  | (_, _, _, _, Green) -> false
  | (_, _, Coffee, _, _) -> false
  
  | (Ukrainian, _, Tea, _, _) -> true
  | (_, _, Tea, _, _) -> false  
  | (Ukrainian, _, _, _, _) -> false
  
  | (_, OldGold, _, Snail, _) -> true
  | (_, OldGold, _, _, _) -> false
  | (_, _, _, Snail, _) -> false
  
  | (_, Kools, _, _, Yellow) -> true
  | (_, _, _, _, Yellow) -> false
  | (_, Kools, _, _, _) -> false
    
  | (_, LuckyStrike, OrangeJuice, _, _) -> true
  | (_, _, OrangeJuice, _, _) -> false
  | (_, LuckyStrike, _, _, _) -> false
    
    
  | (Japanese, Parliament, _, _, _) -> true
  | (_, Parliament, _, _, _) -> false
  | (Japanese, _, _, _, _) -> false 
    
  | _ -> false

let uniq (h1, h2, h3, h4, h5) =
  let n1, s1, d1, o1, c1 = h1 in
  let n2, s2, d2, o2, c2 = h2 in
  let n3, s3, d3, o3, c3 = h3 in
  let n4, s4, d4, o4, c4 = h4 in
  let n5, s5, d5, o5, c5 = h5 in
  (NSet.of_list [ n1 ; n2 ; n3 ; n4 ; n5 ] |> NSet.cardinal) = 5 &&
  (SSet.of_list [ s1 ; s2 ; s3 ; s4 ; s5 ] |> SSet.cardinal) = 5 &&
  (DSet.of_list [ d1 ; d2 ; d3 ; d4 ; d5 ] |> DSet.cardinal) = 5 &&
  (OSet.of_list [ o1 ; o2 ; o3 ; o4 ; o5 ] |> OSet.cardinal) = 5 &&
  (CSet.of_list [ c1 ; c2 ; c3 ; c4 ; c5 ] |> CSet.cardinal) = 5 

let is_street_valid hs =
  uniq hs
  (* fact_2 hs *)
  (* && fact_3 hs *)
  (* && fact_4 hs *)
  (* && fact_5 hs *)
  && fact_6 hs
  (* && fact_7 hs *)
  (* && fact_8 hs *)
  (* && fact_9 hs *)
  (* && fact_10 hs *)
  && fact_11 hs
  && fact_12 hs
  (* && fact_13 hs *)
  (* && fact_14 hs *)
  && fact_15 hs
  
  
let combinations (h1, h2, h3, h4, h5) =
  let c2 a b = [a, b ; b, a] in
  let c3 a b c =
    let ab = c2 a b in
    let bc = c2 b c in
    let ac = c2 a c in
    List.map (fun (x, y) -> c, x, y) ab @
    List.map (fun (x, y) -> a, x, y) bc @
    List.map (fun (x, y) -> b, x, y) ac
  in
  let c4 a b c d = 
    let bcd = c3 b c d in
    let acd = c3 a c d in
    let abd = c3 a b d in
    let abc = c3 a b c in
    List.map (fun (x, y, z) -> a, x, y, z) bcd @
    List.map (fun (x, y, z) -> b, x, y, z) acd @
    List.map (fun (x, y, z) -> c, x, y, z) abd @
    List.map (fun (x, y, z) -> d, x, y, z) abc
  in
  let c5 a b c d e = 
    let bcde = c4 b c d e in
    let acde = c4 a c d e in
    let abde = c4 a b d e in
    let abce = c4 a b c e in
    let abcd = c4 a b c d in
    List.map (fun (w, x, y, z) -> a, w, x, y, z) bcde @
    List.map (fun (w, x, y, z) -> b, w, x, y, z) acde @
    List.map (fun (w, x, y, z) -> c, w, x, y, z) abde @
    List.map (fun (w, x, y, z) -> d, w, x, y, z) abce @ 
    List.map (fun (w, x, y, z) -> e, w, x, y, z) abcd
  in
  c5 h1 h2 h3 h4 h5 
           
let generate_houses () : house list =
  let nats = 
    [ English
    ; Spainiard
    ; Ukrainian
    ; Japanese
    ; Norwegian 
    ] 
  in
  let smokes = 
    [ Kools
    ; OldGold
    ; Chesterfield
    ; LuckyStrike
    ; Parliament
    ] 
  in
  let drink = 
    [ Milk
    ; Water
    ; Coffee
    ; Tea
    ; OrangeJuice
    ] 
  in
  let owns = 
    [ Dog
    ; Horse
    ; Fox
    ; Zebra
    ; Snail
    ] 
  in
  let colors = 
    [ Red
    ; Blue
    ; Yellow
    ; Ivory
    ; Green
    ] 
  in
  List.concat @@ List.map (fun n ->
      List.concat @@ List.map(fun s ->
          List.concat @@ List.map(fun d ->
              List.concat @@ List.map(fun o -> 
                  List.map(fun c ->
                      n, s, d, o, c
                    ) colors
                ) owns
            ) drink 
        ) smokes
    ) nats
  
let houses = generate_houses () 
  |> List.filter is_house_valid


let norwegian = houses 
  |> List.filter (function (Norwegian, _, _, _, _) -> true | _ -> false)
  |> List.filter (function (_, _, Milk, _, _) -> false | _ -> true)

let milk = houses
  |> List.filter (function (_, _, Milk, _, _) -> true | _ -> false)

let houses = houses
  |> List.filter (function (Norwegian, _, _, _, _) -> false | _ -> true)
  |> List.filter (function (_, _, Milk, _, _) -> false | _ -> true)

let c = ref 1

exception Answer of street

let _ = 
  try
    List.iter (fun house1 ->
      List.iter (fun house3 ->
        List.iter (fun house2 ->
          List.iter (fun house4 ->
            List.iter (fun house5 ->
              print_endline ("count = " ^ (string_of_int !c));
              incr c;
              let s = house1, house2, house3, house4, house5 in
              if is_street_valid s then raise (Answer s)
            ) houses
          ) houses 
        ) houses
      ) milk  
    ) norwegian
  with Answer s -> Printf.printf "Answer = \n%s\n" (print_street s)

(*
   1. Solve Zebra puzzle in js or OCaml
2. Implement zebra puzzle the prolog part1
3. Implement zebra puzzle prolog part2

Answer = 
House1 = Nationality = Norwegian, Smoke = Kools, Drink = Water, Owns = Horse, Color = Yellow
House2 = Nationality = Spainiard, Smoke = Kools, Drink = Water, Owns = Dog, Color = Blue
House3 = Nationality = English, Smoke = Kools, Drink = Milk, Owns = Dog, Color = Red
House4 = Nationality = Spainiard, Smoke = Chesterfield, Drink = Water, Owns = Dog, Color = Ivory
House5 = Nationality = Ukrainian, Smoke = Kools, Drink = Coffee, Owns = Fox, Color = Green

real    1m18.874s
user    0m6.571s
sys     0m31.944s
*)
  
  
  
  
  
  
  
  




  
  