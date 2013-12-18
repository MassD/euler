(**

Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

*)

let fib_even_sum n =
  let rec fib a b sum =
    let s = a + b in
    if s > n then sum
    else if s mod 2 = 0 then fib b s (sum+s)
    else fib b s sum
  in 
  fib 1 1 0

