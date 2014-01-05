let fib_dig dig = 
  let rec add_dig acc d1 d2 next =
    let sum = d1+d2+next in
    ((sum mod 10)::acc, sum/10)
  and add (acc, next) num_dig = function
    | [], [] -> 
      if next <> 0 then (List.rev (next::acc), num_dig+1) 
      else (List.rev acc, num_dig)
    | hd::tl, [] | [], hd::tl -> add (add_dig acc hd 0 next) (num_dig+1) (tl,[])
    | hd1::tl1, hd2::tl2 -> add (add_dig acc hd1 hd2 next) (num_dig+1) (tl1,tl2)
  and fib_loop i j t =
    let (fib_num, num_dig) = add ([], 0) 0 (i,j) in
    if num_dig = dig then t+1
    else fib_loop j fib_num (t+1) 
  in 
  fib_loop [1] [1] 2

let _ =
  let t1 = Sys.time() in
  let ans = fib_dig 1000 in
  let t2 = Sys.time() in
  Printf.printf "ans = %d, cost %f sec\n" ans (t2-.t1)
