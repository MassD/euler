(**

Lattice paths

Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

http://projecteuler.net/project/images/p_015.gif

How many such routes are there through a 20×20 grid?

*)

let lattice m n =
  let a = Array.make_matrix m n (-1) in
  let rec lat m n =
    if m > 1 && n > 1 then (
      if a.(m-1).(n-1) <> -1 then a.(m-1).(n-1)
      else 
	let p1 = lat (m-1) n in
	let p2 =  lat m (n-1) in
	let p = p1+p2 in
	a.(m-1).(n-1) <- p;
	a.(n-1).(m-1) <- p;
	p
    )
    else if m <= 1 && n <= 1 then 2
    else if m <= 1 then n + 1
    else m + 1
  in 
  lat m n

let _ = 
  let t1 = Sys.time() in
  let ans = lattice 20 20 in
  let t2 = Sys.time() in
  Printf.printf "lattice 20 = %d, cost %f sec\n" ans (t2-.t1)
