let sieve_primes n =
  let a = Array.init n (fun i -> i+1) in
  a.(0) <- 0;
  let rec mark m i = 
    if i <= n then (a.(i-1) <- 0; mark m (i+m))
    else ()
  and loop i =
    if i > n then ()
    else (
      if a.(i-1) <> 0 then mark i (2*i)
      else ();
      loop (i+1)
    )
  in 
  loop 2;
  a

let take_from_array filter a n =
  let rec take i acc =
    if i = n then acc
    else take (i+1) (a.(i)::acc)
  in 
  take 0 [] |> List.filter filter

let quadratic_primes a_hi b_hi =
  let sieve_prime_a = sieve_primes (b_hi*b_hi + b_hi*a_hi+b_hi) in
  let b_candidates = take_from_array (fun x -> if x = 0 then false else true) sieve_prime_a b_hi |> List.rev in
  let rec fix_a_b i a b =
    let num = i*i + i*a + b in
    if num >= 1 && num <= Array.length sieve_prime_a && sieve_prime_a.(num-1) <> 0 then fix_a_b (i+1) a b
    else i
  and fix_b (max,a_max) a b =
    if a = a_hi then (max,a_max)
    else 
      let next = fix_a_b 0 a b in
      if max < next then fix_b (next,a) (a+1) b
      else fix_b (max,a_max) (a+1) b
  in 
  List.fold_left (
    fun (max,a_max,b_max) b ->
      let next,a_next= fix_b (0,(-b_hi)) (-b_hi) b in
      if max < next then next,a_next,b
      else max,a_max,b_max
  ) (0,0,0) b_candidates
    
let _ =
  let t1 = Sys.time() in
  let _, a, b = quadratic_primes 999 999 in
  let t2 = Sys.time() in
  Printf.printf "ans = %d, cost %f sec\n" (a*b) (t2-.t1)
