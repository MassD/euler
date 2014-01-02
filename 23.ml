(**

A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

*)

let div_sum n =
  let sqrtn = int_of_float (sqrt (float_of_int n)) in
  let rec ds i sum =
    if i > sqrtn then sum
    else if n mod i = 0 then (
      if i*i = n then ds (i+1) sum+i
      else ds (i+1) sum+i+(n/i)
    )
    else ds (i+1) sum
  in 
  ds 2 1

let abundant_list_under k =
  let rec loop i acc =
    if i = 1 then acc
    else if div_sum i > i then loop (i-1) (i::acc)
    else loop (i-1) acc
  in 
  loop (k-1) []

let abundant_sum_under k =
  let abl = abundant_list_under k in
  let a = Array.make (k-1) (false, 0) in
  let rec loop = function
    | [] -> ()
    | hd::tl -> 
      List.iter (fun x -> if hd+x < k then a.(hd+x-1) <- (true, hd+x) else ()) (hd::tl);
      loop tl
  in 
  loop abl;
  Array.fold_left (fun sum (is_as, i) -> if is_as then sum+i else sum) 0 a

let non_abundant_sum_under k = k*(k-1)/2-(abundant_sum_under k)

let _ =
  let t1 = Sys.time() in
  let ans = non_abundant_sum_under 28123 in
  let t2 = Sys.time() in
  Printf.printf "ans = %d, cost %f sec\n" ans (t2-.t1)

