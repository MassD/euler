(**

The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.

*)

module IntMap = Map.Make (struct type t = int let compare = compare end)

let sequence map n =
  let rec sq_loop i l =
    if IntMap.mem i map = true then 
      List.fold_left (fun (num_sq, map) x -> (num_sq+1, IntMap.add x (num_sq+1) map)) (IntMap.find i map, map) l
    else 
      let next = if i mod 2 = 0 then i/2 else 3*i+1 in
      sq_loop next (i::l)
  in 
  sq_loop n []

let max_seq n =
  let map = IntMap.add 1 1 (IntMap.empty) in
  let rec find_max (max,s) i map =
    if i = n then max, s
    else
      let num_sq, map = sequence map i in
      if max < num_sq then find_max (num_sq,i) (i+1) map
      else find_max (max,s) (i+1) map
  in 
  find_max (2,2) 2 map

let max_seq_999999 = 
  let t1 = Sys.time() in
  let max,s = max_seq 999_999 in
  let t2 = Sys.time() in
  Printf.printf "max_seq = %d, starting = %d, cost %f sec\n" max s (t2-.t1)
