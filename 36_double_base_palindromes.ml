(*
  Problem 36, Double-base palindromes

  The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

  Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

  (Please note that the palindromic number, in either base, may not include leading zeros.)
*)

let sum l = List.fold_left (+) 0 l

let base2_to_base10 s =
  let len = String.length s in
  if len = 0 || s.[0] = '0' then 0
  else 
    let rec cal acc b2 i =
      if i < 0 then acc
      else cal (acc+ (if s.[i] = '1' then b2 else 0)) (b2*2) (i-1)
    in 
    cal 0 1 (len-1)

let palindromes d el =
  let rec grow acc i mids =
    if i > d then acc
    else 
      let ml = List.map (fun mid -> List.map (fun x -> x^mid^x) el) mids |> List.flatten in
      grow (List.rev_append ml acc) (i+2) ml 
  in 
  let pl1 = grow [] 0 [""] in
  let pl2 = List.map (fun x -> grow [] 1 [x]) el |> List.flatten |> List.rev_append el in
  List.rev_append pl1 pl2
    
let s10 = ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9"]

let s2 = ["0";"1"]

let max_d_base10 n = ceil (log10 (float_of_int n)) |> int_of_float
let max_d_base2 n = ceil ((log (float_of_int n)) /. (log 2.)) |> int_of_float

let double_base_palindromes n =
  let d10 = max_d_base10 n and d2 = max_d_base2 n in
  let a = Array.make n false in
  let p10 = palindromes d10 s10 |> List.map (fun x -> if x.[0] = '0' then 0 else int_of_string x) in
  List.iter (fun x -> if x < n then a.(x) <- true else ()) p10;
  let p2 = palindromes d2 s2 |> List.map base2_to_base10 in
  let final = List.filter (fun x -> x <> 0 && x < n && a.(x)) p2 in
  final


let n = 1_000_000


