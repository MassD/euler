(*
  Problem 35, Circular primes

  The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

  There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

  How many circular primes are there below one million?
*)

(*
  1. create sieve prime array under 1 million
  2. get one true in sieve (prime), check all possible rotations, if all rotations are primes, count them; anyway, mark them false
*)

let sieve n =
  let a = Array.make n true in
  a.(0) <- false;
  a.(1) <- false;
  let rec mark_false i j =
    if i*j >= n then ()
    else (a.(i*j) <- false; mark_false i (j+1))
  in 
  for i = 2 to n-1 do
    if a.(i) then mark_false i 2
    else ()
  done;
  a

let all_digits i =
  let rec collect acc n =
    if n = 0 then acc
    else collect ((string_of_int (n mod 10))::acc) (n/10)
  in 
  collect [] i

let rotations i =
  let l = all_digits i in
  let rec generate acc out = function
    | [] -> acc
    | hd::tl as l -> generate ((l@out)::acc) (out@[hd]) tl
  in 
  generate [] [] l |> List.map (fun x -> String.concat "" x |> int_of_string)

let rec destutter = function
  | [] -> []
  | hd1::(hd2::_ as tl) when hd1 = hd2 -> destutter tl
  | hd::tl -> hd::destutter tl

let rm_dup l = List.sort compare l |> destutter

let count_circular_primes n =
  let s = sieve n in
  let check rots = List.fold_left (fun acc x -> acc&&s.(x)) true rots in
  let rec check_all acc count i =
    if i >= n then count
    else if s.(i) then 
      let rots = rotations i in
      let b = check rots in
      List.iter (fun x -> s.(x) <- false) rots;
      if b then check_all (rots::acc) (count+List.length (rm_dup rots)) (i+1)
      else  check_all acc count (i+1)
    else check_all acc count (i+1)
  in 
  check_all [] 0 0
