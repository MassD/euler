let div_sum n =
  let sqrtn = int_of_float (sqrt (float_of_int n)) in
  let rec ds i sum =
    if i > sqrtn then sum
    else if n mod i = 0 then (
      if i*i = n then ds (i+1) sum+i
      else ds (i+1) sum+i+(n/i)
    )
    else ds (i+1) sum
  in 
  (false, ds 2 1)

let amicable_sum k =
  let a = Array.init k (fun i -> i+1) in
  let b = Array.map div_sum a in
  Array.iteri (
    fun i (_, x) ->
      if i+1 = x || x >= k then ()
      else if i+1 = snd b.(x-1) then (b.(i) <- (true, x); b.(x-1) <- (true, i+1))
      else ()
  ) b;
  Array.fold_left (fun acc (is_ami, x) -> if is_ami then acc+x else acc) 0 b
