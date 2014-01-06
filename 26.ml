let reciprocal_cycle num =
  let a = Array.make num 0 in
  let rec rc next c =
    let q = next/num and r = next mod num in
    if q = 0 && r = 0 then 0
    else (
      if a.(r) <> 0 then c-a.(r)
      else (a.(r) <- c; rc (r*10) (c+1))
    )
  in 
  rc 1 1

let max_rc n = 
  let rec max_loop (max,d) i =
    if i = n then (max,d)
    else 
      let rc = reciprocal_cycle i in
      if max < rc then max_loop (rc,i) (i+1)
      else max_loop (max,d) (i+1)
  in 
  max_loop (0,2) 3


let _ =
  let t1 = Sys.time() in
  let ans = snd(max_rc 100000) in
  let t2 = Sys.time() in
  Printf.printf "ans = %d, cost %f sec\n" ans (t2-.t1)
