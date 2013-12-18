(**
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

*)

let decompose n l =
  let rec decomp m = function
    | [] -> if m <> 1 then m::l else l
    | hd::tl -> if m mod hd = 0 then decomp (m/hd) tl else decomp m tl
  in 
  decomp n l

let min_even_div' n =
  let rec iter i acc =
    if i > n then acc
    else iter (i+1) (decompose i acc)
  in 
  List.fold_left ( * ) 1 (iter 1 [1])
