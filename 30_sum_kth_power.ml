(**

Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

1634 = 14 + 64 + 34 + 44
8208 = 84 + 24 + 04 + 84
9474 = 94 + 44 + 74 + 44
As 1 = 14 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

*)

(*
  Analyse first in terms of Math.
  a^4 + b^4 + c^4 + d^4 = a*1000 + b*100 + c*10 + d 
  We can obviously use bruteforce, increase each digit from 0 to 9, and also increase the number of digits.
  
  But one important question is: what is the bound?
  If given kth powers, then the bound can be inducted by:
  If at some point `d * 9^k` is smaller than `1 * 10^d` (the max left < the min right), then we should totally stop.
  
  d * 9^k < 10^d => d > logd + klog9, then the first integer d making this inequation stand is the bound we are seeking for.

  We can also do some optimisation:
  1. store all 0^k, 1^k, ..., 9^k
  2. store all results level by level. For example, we store all `x*10+y` after trying, then when reaching 3 digits, we just generate `z*100` and pairing it to all `x*10+y`.

*)

let max_d k =
  let kf = float_of_int k in
  let rec cal d =
    if d *. (9. ** kf) < 10. ** (d-.1.) then d
    else cal (d +. 1.)
  in 
  cal 2.

(* fuse means hd1 from list1, f hd1 everyone of list2 *)
let fuse f l1 l2 =
  let rec collect acc = function
    | [], _ -> List.rev acc
    | hd::tl, [] -> collect acc (tl, l2)
    | hd1::tl1, hd2::tl2 -> let r = f hd1 hd2 in collect (r::acc) (hd1::tl1, tl2)
  in 
  collect [] (l1,l2)

(* generate 0^k, 1^k, ..., n^k as an array *)
let powers k n = Array.init n (fun i -> (float_of_int i) ** (float_of_int k) |> int_of_float) 

(* generate (i^k, factor * i) list *)
let level ps factor = Array.init 10 (fun i -> i) |> Array.to_list |> List.map (fun x -> (ps.(x), factor * x)) 

(* in this problem, it means (j^k+i^k, factor1*j+factor2*i *)
let f (a1,a2) (b1,b2) = a1+b1, a2+b2

let sum l =
  let rec collect acc = function
    | [] -> acc
    | hd::tl -> collect (acc+hd) tl
  in 
  collect 0 l

let sum_kth_power k =
  let d = max_d k in
  let ps = powers k 10 in
  let rec collect acc i pre =
    if i >= d then List.rev (List.filter (fun (x,y) -> x = y && x > 9) acc) |> List.map fst
    else 
      let factor = 10. ** (i-.1.) |> int_of_float in (* get factor 10*i (10, or 100, etc) based on i *)
      let new_level = level ps factor in (* get new (x^k, factor*x) list *)
      let fused = fuse f (List.tl new_level) pre in (* fuse combine the new_level with previous result *) 
      collect (List.rev_append fused acc) (i+.1.) (List.rev_append (fuse f [List.hd new_level] pre) fused)
  in 
  collect [] 2. (level ps 1)
  
  
