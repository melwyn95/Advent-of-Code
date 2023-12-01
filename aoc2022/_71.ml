let rec gcd a b = if a = 0 then b else gcd (b mod a) a
    
module Fraction = struct
  type t = { n : int ; d : int }

  let make n d = { n ; d }
         
  let compare f1 f2 =
    let f1 = float_of_int f1.n /. float_of_int f1.d in
    let f2 = float_of_int f2.n /. float_of_int f2.d in
    Float.compare f1 f2
  
  let reduce f =
    let { n ; d } = f in
    let gcd = gcd n d in
    if gcd <> 1 
    then { n = (n / gcd) ; d = (d / gcd) }
    else f
end

module FSet = Set.Make (Fraction)

let rec join xs ys =
  match xs with
  | [] -> ys
  | x :: xs -> join xs (x :: ys)

let n = 1000_000

let nums = 
  let rec aux i xs = 
    if i = n then n :: xs else aux (i + 1) (i :: xs)
  in
  aux 1 [] 

let one = nums
|> List.map (fun d -> 
  if 1 < d && gcd 1 d = 1 then 
    let f = Fraction.make 1 d in
    let f = Fraction.reduce f in
    Some f
  else None
) 
|> List.filter Option.is_some
|> List.map Option.get

(* let two = nums
|> List.map (fun d -> 
  if 2 < d && gcd 2 d = 1 then 
    let f = Fraction.make 2 d in
    let f = Fraction.reduce f in
    Some f
  else None
)
|> List.filter Option.is_some
|> List.map Option.get

let three = nums
|> List.map (fun d -> 
  if 3 < d && gcd 3 d = 1 then 
    let f = Fraction.make 3 d in
    let f = Fraction.reduce f in
    Some f
  else None
)
|> List.filter Option.is_some
|> List.map Option.get *)


  
let fractions =
  List.concat [ one ; [] ; [] ]
  |> List.sort_uniq Fraction.compare

let three_by_seven = Fraction.make 3 7

let () = Printf.printf "here\n%!"

let find fractions =
  let rec aux p fractions =
    match fractions with
    | [] -> failwith "not found"
    | f :: fs -> if f = three_by_seven then p else aux f fs
  in
  match fractions with
  | f :: fs -> aux f fs
  | [] -> failwith "not found"

let Fraction.{ n ; d } = find fractions
let () = Printf.printf "%d / %d \n" n d