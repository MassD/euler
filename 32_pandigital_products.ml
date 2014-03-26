(*

  Pandigital products. Problem 32

  We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

  The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

  Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

  HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

*)

(* 
   First, we need to check valid number of digits for each part, as 
   1. the total number is 9
   2. we need to avoid duplicate, so we assume the multiplicand <= multiplier which means number of digits of multiplicand < number of digits of multiplier. 
   3. the number of digits of both left parts should be able to produce number of digits of the right product part.
   
   So the possible number of digits partitions are: (1, 4, 4) and (2, 3, 4)

   Then we can pick x out of n, then pick y out of n-x, then pick z out of n-x-y, and see whether (pick x n) * (pick y (n-x)) is in (pick z (n-x-y)). Note that for each pick (partition), we also need to do permutation on it.

*)

let s = Array.init 9 (fun i -> i+1) |> Array.to_list
let splits = [(1,4,4); (2,3,4)]

let (--) l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1
let inter l1 l2 = List.filter (fun x -> List.mem x l2) l1

let rec pick k = function
  | [] -> []
  | _ when k <= 0 -> []
  | l when k = 1 -> List.map (fun x -> [x]) l
  | hd::tl -> 

    List.map (fun x -> hd::x) (pick (k-1) tl) @ (pick k tl)

let rec permutation = function
  | [] -> []
  | x::[] -> [[x]]
  | l -> List.fold_left (fun acc x -> List.map (fun y -> x::y) (permutation (l--[x])) @ acc) [] l

let num l = 
  let rec cal acc factor = function
    | [] -> acc
    | hd::tl -> cal (acc+factor*hd) (factor*10) tl
  in 
  cal 0 1 l

let product l1 l2 = (num l1) * (num l2)

let fuse f l1 l2 = List.fold_left (fun acc x -> List.rev_append (List.rev_map (fun y -> f x y) l2) acc) [] l1

let sum l = List.fold_left (fun acc x -> x + acc) 0 l
let rm_dup l =
  let rec collect acc = function
    | [] -> List.rev acc
    | hd::tl -> collect (hd::acc) (List.filter ((<>) hd) tl)
  in 
  collect [] l

let pandigital_products_sum =
  let fix_xy lx ly =
    let lzs = permutation ((s--lx)--ly) |> List.map num in
    fuse product (permutation lx) (permutation ly) |> inter lzs
  in 
  let fix_x y lx = 
    pick y (s--lx) |> List.fold_left (fun acc ly -> List.rev_append (fix_xy lx ly) acc) []
  in
  let xyz (x,y,z) = 
    pick x s |> List.fold_left (fun acc lx -> List.rev_append (fix_x y lx) acc) []
  in 
  List.fold_left (fun acc p -> List.rev_append (xyz p) acc) [] splits |> rm_dup |> sum
