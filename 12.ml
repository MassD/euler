let num_div n =
  let sqrtn = int_of_float (sqrt (float_of_int n)) in
  let rec loop i count =
    if i >= sqrtn then (
      if n mod i = 0 then 2*count+1
      else 2*count
    )
    else if n mod i = 0 then loop (i+1) (count+1)
    else loop (i+1) count
  in 
  loop 1 0

let rec over_500_div i =
  let tn = i*(i+1)/2 in
  (*let sqrt_tn = int_of_float (sqrt (float_of_int tn)) in*)
  if (*tn = sqrt_tn * sqrt_tn &&*) num_div tn > 500 then
    tn, (num_div tn)
  else over_500_div (i+1)

let _ =
  let t1 = Sys.time() in
  let ans = over_500_div 250 in
  let t2 = Sys.time() in
  Printf.printf "ans = %d, num_div = %d, cost %f s\n" (fst ans) (snd ans) (t2-.t1)
  
  
