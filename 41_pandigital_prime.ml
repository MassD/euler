(*
  Problem 41, Pandigital prime

  We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

  What is the largest n-digit pandigital prime that exists?
*)

(*
  For any prime problem, think of sieve array first
  1. generate all primes up to 9 digits
  2. collect all primes in descending order
  3. for each prime, check whether it is pandigital

  Note that if all digits' sum can be divided by 3, then it is impossible to be a prime. So the only possible number of digits are 4 and 7. So try 9_999_999 first.
*)

let sieve n =
  let a = Array.make n true in
  a.(0) <- false;
  a.(1) <- false;
  let rec mark i j =
    if i*j >= n then ()
    else (a.(i*j) <- false; mark i (j+1))
  in 
  for i = 2 to n-1 do
    if a.(i) then mark i 2
  done;
  a

let pans = [|1;12;123;1234;12345;123456;1234567;12345678;123456789|]

let is_pan n =
  let rec to_list (l,d) i =
    if i = 0 then List.sort compare l,d
    else to_list (i mod 10::l, d+1) (i/10)
  in 
  let rec of_list l = List.fold_left (fun acc x -> 10*acc+x) 0 l in
  let l,d = to_list ([], 0) n in
  pans.(d-1) = of_list l

let max_pandigital_prime =
  let n = 9_999_999 in
  let a = sieve n in
  let rec check i =
    if i < 2 then None
    else if a.(i) && is_pan i then Some i
    else check (i-1)
  in 
  check (n-1)
