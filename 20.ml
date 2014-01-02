let factorial_sum l m =
  let ml, a = 
    List.map (fun x -> m*x) l 
	|> List.rev 
	|> List.fold_left (
	  fun (acc_l, a) x -> 
	    ((x+a) mod 10)::acc_l, (x+a)/10
	) ([],0)
  in 
  let rec num_to_list num acc =
    if num = 0 then acc
    else if num < 10 then num::acc
    else num_to_list (num/10) ((num mod 10)::acc)
  in 
  num_to_list a ml

let sum = List.fold_left (+) 0

let factorial_sum_k k =
  let rec ms_loop acc m i =
    if i > k then acc
    else ms_loop (factorial_sum acc m) (m+1) (i+1)
  in 
  ms_loop [1] 1 1 |> sum

let _ =
  let t1 = Sys.time() in
  let ans = factorial_sum_k 100 in
  let t2 = Sys.time() in
  Printf.printf "100! ans = %d, cost %f sec\n" ans (t2-.t1)
  
