(*
  Problem 37, Truncatable primes

  The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

  Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

  NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
*)

let sieve n =
  let a = Array.init n (fun i -> not (i mod 2 = 0)) in
  a.(0) <- false;
  a.(1) <- false;
  a.(2) <- true;
  let rec disable i j =
    let k = i * j in
    if k >= n then ()
    else (a.(k) <- false; disable i (j+1))
  in 
  for i = 3 to n-1 do
    disable i 2
  done;
  a

let find_truncatable_primes =
  let target = 1_000_000 in
  let a = sieve target in
  let degree10 n = 10. ** (n |> float_of_int |> log10 |> int_of_float |> float_of_int) |> int_of_float in
  let rec is_truncatable m n d =
    if m = 0 || n = 0 then true
    else if a.(m) && a.(n) then is_truncatable (m/10) (n mod d) (d/10)
    else false
  in 
  let rec find acc c i =
    if c = 11 || i >= target then acc
    else if is_truncatable i i (degree10 i) then find (i::acc) (c+1) (i+2)
    else find acc c (i+2)
  in 
  find [] 0 23

let sum = List.fold_left (+) 0 find_truncatable_primes
