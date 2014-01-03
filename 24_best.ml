(**

A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

*)

let factorial n = 
  let rec fac acc i =
    if i = 1 then acc
    else fac (i*acc) (i-1)
  in 
  fac 1 n

let kth_permutation k l =
  let n = List.length l in
  let rec find_kth k unum level i candidates rest =
    match candidates with
      | [] -> raise Not_found
      | hd::tl ->
	let num = unum*i in
	if num < k then find_kth k unum level (i+1) tl (hd::rest)
	else if num = k then (hd::tl)@rest
	else hd::(find_kth (k-(num-unum)) (unum/level) (level-1) 1 (List.rev_append rest tl) [])
  in 
  let total = factorial n in
  if k = total then List.rev l
  else if k > total then raise Not_found
  else find_kth k (total/n) (n-1) 1 l []


let num_string_to_list ns = 
  let buf = List.length ns |> Buffer.create in
  List.iter (fun x -> Buffer.add_string buf (string_of_int x)) ns;
  Buffer.contents buf

let _ =
  let t1 = Sys.time() in
  let ans = kth_permutation 1_000_000 [0;1;2;3;4;5;6;7;8;9] in
  let t2 = Sys.time() in
  Printf.printf "ans = %s, cost %f sec\n" (num_string_to_list ans) (t2-.t1)
