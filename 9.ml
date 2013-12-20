let result =
  let sb = int_of_float (sqrt 500000.) in
  for b = 293 to 999 do
    let a = (1000*b-500000)/(b-1000) in
    let c = 1000-a-b in
    if a > 1 && c > 1 && a*a + b*b = c*c then Printf.printf "a = %d, b = %d, c = %d, abc = %d\n" a b c (a*b*c)
    else ()
  done 
