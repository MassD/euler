(**

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?

*)

let max_prime_factor n =
  let rec factors acc m d =
    let d2 = d * d in
    if m > 1 && d2 < m then begin
      if m mod d = 0 then factors (d::acc) (m/d) d
      else factors acc m (d+1)
    end 
    else if d2 > m && m > 1 then m::acc
    else acc
  in 
  List.fold_left (fun a c -> if a < c then c else a) 1 (factors [] n 2)


