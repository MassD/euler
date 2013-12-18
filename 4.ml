(**

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.

*)

let rev_int n = 
  let rec rev acc n = 
    if n = 0 then acc
    else rev (acc*10 + n mod 10) (n/10)
  in 
  rev 0 n

let palindrome n = n = (rev_int n)

let max_palindrome n1 n2 = 
  let rec iter i j max =
    if i = n1-1 then max
    else 
      let m = i*j in
      if palindrome m && fst max < m then (
	if j = n1 then iter (i-1) n2 (m, (i,j))
	else iter i (j-1) (m, (i,j))
      )
      else (
	if j = n1 then iter (i-1) n2 max
	else iter i (j-1) max
      )
  in 
  iter n2 n2 (0,(0,0))

let max_palindrome_faster n1 n2 = 
  let rec iter i j =
    if i = n1-1 then None
    else 
      let m = i*j in
      if palindrome m then Some (m, i, j)
      else 
	if j mod 100 = 0 then iter (i-1) (i-2)
	else iter i (j-1)
  in 
  iter n2 (n2-1)
      
    
