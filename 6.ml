let sum_square_diff n =
  let s1 = n * n * (n+1) * (n+1) / 4 in
  let rec sum_sq acc i =
    if i > n then acc
    else sum_sq (acc - i*i) (i+1)
  in 
  sum_sq s1 1
