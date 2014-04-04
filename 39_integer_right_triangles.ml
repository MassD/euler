(*
  Problem 39, Integer right triangles

  If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

  {20,48,52}, {24,45,51}, {30,40,50}

  For which value of p ≤ 1000, is the number of solutions maximised?
*)

(*
  a^2 + b^2 = c^2
  a + b + c = p
  a <= b <= c -> a <= p/3
*)

let validate a b c = a*a + b*b = c*c

let solutions p =
  let rec find count a b =
    if a > p / 3 then count
    else 
      let c = p - a - b in
      if c < b then find count (a+1) (a+1)
      else if validate a b c then ((*Printf.printf "good, a=%d,b=%d\n" a b;*)find (count+1) a (b+1))
      else find count a (b+1)
  in 
  find 0 1 1
   
let max_solution n =
  let rec find (m, mp) p =
    if p < 3 then mp
    else 
      let np = solutions p in
      if m < np then find (np, p) (p-1)
      else find (m, mp) (p-1)
  in 
  find (0, 0) n
