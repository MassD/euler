(*
  Problem 40, Champernowne's constant

  An irrational decimal fraction is created by concatenating the positive integers:

  0.123456789101112131415161718192021...

  It can be seen that the 12th digit of the fractional part is 1.

  If dn represents the nth digit of the fractional part, find the value of the following expression.

  d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
*)

(* 
   Champernowne's constant is to connect every possitive integers together in a row.
   1 2 3 4 5 6 7 8 9 10 11 12 13 ...

   The key part here is to find **how many digits within a given range**.
   the number of digits pattern is: 9 * 1 + 90 * 2 + 900 * 3
   So if we ask for the nth digit, we can reduce n by the terms above one by one, and use / and mod get the one we want
*)

let int_to_list n =
  let rec collect acc i =
    if i = 0 then acc
    else collect (i mod 10::acc) (i/10)
  in 
  collect [] n

let kth i k = List.nth (int_to_list i) (k-1)
 
let nth_digit n =
  let rec find m i r n =
    let k = m * i in
    if n < k && n mod i = 0 then (r+n/i) mod 10 
    else if n < k then kth (r+n/i+1) (n mod i) 
    else find (m*10) (i+1) (r+m) (n-k)
  in 
  find 9 1 0 n

let nth_digit_champernowne = 
  let ns = [1;10;100;1000;10000;100000;1000000] in
  List.map nth_digit ns |> List.fold_left ( * ) 1
  


