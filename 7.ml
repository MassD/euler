let is_prime = function
  | 1 -> false
  | 2 -> true
  | _ as x when x mod 2 = 0 -> false
  | _ as x ->
    let rec check i =
      if i <= 1 then true
      else if x mod i = 0 then false
      else check (i-2)
    in 
    let s = int_of_float (sqrt (float_of_int x)) in
    if s mod 2 = 0 then check (s-1)
    else check s

let kth_prime k =
  if k = 1 then 2
  else 
    let rec find_prime i c =
      if is_prime i && c+1 = k then i
      else if is_prime i then find_prime (i+2) (c+1)
      else find_prime (i+2) c
    in 
    find_prime 3 1
