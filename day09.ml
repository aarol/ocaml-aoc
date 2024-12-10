open Core

let digit_from_char c = int_of_char c - 48

(* type block_value = Free | Sized of int
   type block = { len : int; value : block_value } *)
(*
   let to_block_list line =
     Array.mapi line ~f:(fun i c ->
         if i % 2 = 0 then { len = digit_from_char c; value = Sized (i / 2) }
         else { len = digit_from_char c; value = Free }) *)

let to_str_list line =
  Array.concat_mapi line ~f:(fun i c ->
      if i % 2 = 0 then
        Array.init (digit_from_char c) ~f:(fun _ -> string_of_int (i / 2))
      else Array.init (digit_from_char c) ~f:(fun _ -> "."))

let part1 line =
  let arr = to_str_list line in

  let rec aux i last_i arr =
    if last_i < i then 0
    else
      let x = arr.(i) in
      if not @@ String.equal x "." then
        (int_of_string x * i) + aux (i + 1) last_i arr
      else
        (* "." at i *)
        let last = arr.(last_i) in
        if String.equal last "." then aux i (last_i - 1) arr
        else (int_of_string last * i) + aux (i + 1) (last_i - 1) arr
  in

  aux 0 (Array.length arr - 1) arr |> Utils.print_int;

  ()

(* let part2 line =
  let arr = to_str_list line in

  let rec checksum_of_range a b =
    if a = b then 0
    else (int_of_string arr.(a) * a) + checksum_of_range (a + 1) b
  in

  let rec first_index_of_file start =
    if start < 1 || (not @@ String.equal arr.(start - 1) ".") then start
    else first_index_of_file (start - 1)
  in
  
  let find_free length = 
    for i = 0 to Array.length arr - length do
      if String.equal arr.(i) "." 
      
      done
      in

  let rec aux right_i arr =
    if right_i = 0 then 0
    else
      let x = arr.(right_i) in

      if not @@ String.equal x "." then
        let left_i = first_index_of_file right_i in
        let length = right_i - left_i in

        
      else aux (right_i - 1) arr
  in

  aux (Array.length arr - 1) arr |> Utils.print_int;

  () *)

let () =
  let lines = Utils.read_string "input/day09.txt" |> String.to_array in

  part1 lines;
  (* part2 lines *)
