let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let str_to_num_list s = Str.split (Str.regexp_string " ") s |> List.map int_of_string |> Array.of_list

let num_tri_ary = List.map str_to_num_list |> Array.of_list

let left_lean_sum_ary nta =
  let h = Array.length nta in
  let a = Array.make_matrix h h 0 in
  let rec diag_sum i j sum =
    a.(i).(j) <- nta.(
  for i = 0 to h-1 do
    

let max_sum nt =
  let max = ref 0 in
  let rec level_loop ci sum = function
    | [] -> if sum > !max then max:=sum else ()
    | a::tl -> (
      level_loop ci (sum+a.(ci)) tl;
      if ci+1 < Array.length a then level_loop (ci+1) (sum+a.(ci+1)) tl
      else ()
    )
  in level_loop 0 0 nt;
  !max

(*
let _ =
  let t1 = Sys.time() in
  let small = read_file "./triangle_small.txt" |> num_tri_ary |> max_sum in
  let t2 = Sys.time() in
  Printf.printf "small = %d, cost %f sec" small (t2-.t1);
  let large = read_file "./triangle.txt" |> num_tri_ary |> max_sum in
  let t3 = Sys.time() in
  Printf.printf "large = %d, cost %f sec" large (t3-.t2);
*)
  
  
