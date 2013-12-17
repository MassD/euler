(**

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.

*)

let rev_int n = 
  let rec rev acc n =
    if n = 0 then acc
    else rev (acc*10+n mod 10) (n/10)
  in 
  rev 0 n

let is_palindrome n = n = (rev_int n)

let max_palindrome num_dig =
  let max_num = (int_of_float (10. ** (float_of_int num_dig))) - 1 in
  let rec find subsum i j =
    if subsum = max_num then None
    else if i < 0 then 
      let new_subsum = subsum+1 in
      let new_i = new_subsum/2 in
      find new_subsum new_i (new_subsum-new_i)
    else 
      let x, y = max_num-i, max_num-j in
      let r = x * y in
      if is_palindrome r then Some (r, x, y)
      else find subsum (i-1) (j+1)
  in 
  find 0 0 0


