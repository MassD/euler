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

let num_tri l = List.map str_to_num_list l |> Array.of_list

let max x y = if x > y then x else y

let max_sum nt =
  let rec from_bottom i j =
    if i = 0 then nt.(0).(0)
    else if j = i then from_bottom (i-1) 0
    else (
      nt.(i-1).(j) <- nt.(i-1).(j)+(max nt.(i).(j) nt.(i).(j+1));
      from_bottom i (j+1)
    )
  in 
  from_bottom ((Array.length nt)-1) 0

let _ =
  let t1 = Sys.time() in
  let small = read_file "./triangle_small.txt" |> num_tri |> max_sum in
  let t2 = Sys.time() in
  Printf.printf "small = %d, cost %f sec\n" small (t2-.t1);
  let large = read_file "./triangle.txt" |> num_tri |> max_sum in
  let t3 = Sys.time() in
  Printf.printf "large = %d, cost %f sec\n" large (t3-.t2);

  
  
