(**

A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

*)

let kth_permutation k l =
  let rec permutate c = function 
    | [] -> c,[]
    | hd::[] -> c+1, [hd]
    | _ as l -> fix_first c [] l
  and fix_first c used = function
    | [] -> c, []
    | hd::tl ->
      let c, acc = permutate c (List.rev_append used tl) in
      if c = k then c, hd::acc
      else fix_first c (hd::used) tl
  in 
  let buf = Buffer.create 10 in
  List.iter (fun x -> Buffer.add_string buf (string_of_int x)) (snd (permutate 0 l));
  Buffer.contents buf

let _ =
  let t1 = Sys.time() in
  let ans = kth_permutation 1_000_000 [0;1;2;3;4;5;6;7;8;9] in
  let t2 = Sys.time() in
  Printf.printf "ans = %s, cost %f sec\n" ans (t2-.t1)
