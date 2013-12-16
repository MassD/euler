(**

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.

*)
let sum_multi a b n =
  let rec sum acc m i j =
    let r = m * i in
    if r >= n then acc
    else if r mod j = 0 then sum acc m (i+1) j
    else sum (r+acc) m (i+1) j
  in 
  (sum 0 a 0 b) + (sum 0 b 0 a) + (sum 0 (a*b) 0 n)
