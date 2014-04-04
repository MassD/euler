(*
  Problem 38, Pandigital multiples

  Take the number 192 and multiply it by each of 1, 2, and 3:

  192 × 1 = 192
  192 × 2 = 384
  192 × 3 = 576
  By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

  The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

  What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?
*)

(* 
   Analyse first.
   The only possible case is 9aaa bbbbb, where 9aaa * 2 = bbbbb
*)

let norm i =
  let rec of_list acc = function
    | [] -> acc
    | hd::tl -> of_list (acc*10+hd) tl
  in
  let rec to_list acc i =
    if i = 0 then acc
    else to_list ((i mod 10)::acc) (i/10)
  in 
  List.sort compare (to_list [] i) |> of_list 0

let pandigital_multiples =
  let rec find i = 
    if i < 9123 then failwith "not_found"
    else 
      let k = i * 100002 in
      if norm k = 123456789 then k
      else find (i-1)
  in 
  find 9876
