let mul_sum l m =
  let ml, a = 
    List.map (fun x -> m*x) l 
	|> List.rev 
	|> List.fold_left (
	  fun (acc_l, a) x -> 
	    ((x+a) mod 10)::acc_l, (x+a)/10
	) ([],0)
  in 
  if a <> 0 then a::ml
  else ml

let sum = List.fold_left (+) 0

let mul_sum_n_k n k =
  let rec ms_loop acc i =
    if i > k then acc
    else ms_loop (mul_sum acc n) (i+1)
  in 
  ms_loop [1] 1 |> sum

let _ =
  let t1 = Sys.time() in
  let ans = mul_sum_n_k 2 1000 in
  let t2 = Sys.time() in
  Printf.printf "2^1000 ans = %d, cost %f sec\n" ans (t2-.t1)
  
