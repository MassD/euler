(*
  Problem 34, Digit factorials

  145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

  Find the sum of all numbers which are equal to the sum of the factorial of their digits.

  Note: as 1! = 1 and 2! = 2 are not sums they are not included.
*)

let fac_9 =
  let n = 9 in
  let rec collect acc pre i =
    if i > n then List.rev acc
    else collect (i*pre::acc) (i*pre) (i+1)
  in 
  collect [1] 1 1 |> Array.of_list

let max_digit = 
  let rec find k p10 =
    if p10 > 9 * k * fac_9.(9) + 1 then k-1
    else find (k+1) (p10*10)
  in 
  find 1 100

let list_of_int i =
  let rec collect n acc =
    if n = 0 then acc
    else collect (n/10) (n mod 10::acc)
  in 
  collect i []

let is_digit_fac i = i = List.fold_left (fun acc x -> acc + fac_9.(x)) 0 (list_of_int i)

let digit_fac_sum = 
  let rec collect sum i =
    if i > 100_000 then sum
    else if is_digit_fac i then collect (sum+i) (i+1)
    else collect sum (i+1)
  in 
  collect 0 10
