let is_prime = function
  | 1 -> false
  | 2 -> true
  | _ as x when x mod 2 = 0 -> false
  | _ as x ->
    let rec check i =
      if i <= 1 then true
      else if x mod i = 0 then false
      else check (i-2)
    in 
    let s = int_of_float (sqrt (float_of_int x)) in
    if s mod 2 = 0 then check (s-1)
    else check s

let sum_prime n =
  let rec sum k acc =
    if k > n then acc
    else 
      if is_prime k then sum (k+2) (acc+k)
      else sum (k+2) acc
  in
  sum 3 2

let sieve_sum_prime n =
  let markers = Array.make (n-1) 0 in
  let rec mark num i =
    if i < n-2 then (markers.(i) <- 1; mark num (i+num))
    else ()
  in
  Array.iteri (
    fun num x -> 
      if x = 0 then (let num = num+2 in mark num (2*num-2); markers.(n-2) <- markers.(n-2)+num)
      else ()
  ) markers;
  markers.(n-2)

let sieve_sum_prime' n =
  let markers = Array.make (n-1) 0 in
  markers.(n-2) <- 2;
  let rec mark num i =
    if i < n-2 then (markers.(i) <- 1; mark num (i+num))
    else ()
  in
  let rec sieve num =
    if num > n then ()
    else ( 
      if markers.(num-2) = 0 then (mark num (2*num-2); markers.(n-2) <- markers.(n-2)+num)
      else ();
      sieve (num+2)
    )
  in 
  sieve 3;
  markers.(n-2)


let _ =
  (*let s1 = Sys.time() in
  let r1 = sum_prime 2000000 in*)
  let e1 = Sys.time() in 
  let r2 = sieve_sum_prime 2000000 in
  let e2 = Sys.time() in
  (*let r3 = sieve_sum_prime' 2000000 in
  let e3 = Sys.time() in*)
  (*Printf.printf "Brute-force answer: %d, cost %f ms\n" r1 (1000.*.(e1 -.s1));*)
  Printf.printf "Sieve answer: %d, cost %f ms\n" r2 (1000.*.(e2 -. e1));
  (*Printf.printf "Sieve' answer: %d, cost %f ms\n" r3 (1000.*.(e3 -. e2))*)
