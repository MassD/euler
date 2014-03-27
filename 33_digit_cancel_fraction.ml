(*
  Problem 33, Digit canceling fractions
  
  The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

  We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

  There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

  If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
*)

(* The only important thing here is to use gcd to simplify numerator and denominator *)

let digits = Array.init 9 (fun i -> i+1) |> Array.to_list
let (--) l x = List.filter ((<>) x) l
let concat_map f l =
  let rec collect acc = function
    | [] -> List.rev acc
    | hd::tl -> collect (List.rev_append (f hd) acc) tl
  in 
  collect [] l

let rec gcd a b = if a = b then a else if a > b then gcd (a-b) b else gcd a (b-a)

let rec pick k = function
  | [] -> []
  | _ when k = 0 -> []
  | l when k = 1 -> List.rev_map (fun x -> [x]) l
  | hd::tl -> List.rev_append (List.map (fun y -> hd::y) (pick (k-1) tl)) (pick k tl)

let rec permutate = function
  | [] -> []
  | hd::[] -> [[hd]]
  | l -> List.fold_left (fun acc x -> List.rev_append (List.map (fun y -> x::y) (permutate (l--x))) acc) [] l

let candidates = pick 2 digits |> concat_map permutate 

let div x y = (float_of_int x) /. (float_of_int y)

let build i = function
  | x::y::[] -> 
    let xdy = div x y in 
    if xdy < 1. then [i*10+x, i*10+y;i*10+x,y*10+i;x*10+i,i*10+y;x*10+i,y*10+i] |> List.filter (fun (a,b) -> div a b = xdy) 
    else []
  | _ -> failwith "Wrong"

let digit_cancel =
  let rec collect acc i =
    if i > 9 then acc
    else collect (List.rev_append (concat_map (build i) candidates) acc) (i+1) 
  in 
  let all = collect [] 1 in
  let numerator_product = List.fold_left (fun acc (x,y) -> acc * x) 1 all in
  let denominator_product = List.fold_left (fun acc (x,y) -> acc * y) 1 all in
  denominator_product / (gcd numerator_product denominator_product)
  

