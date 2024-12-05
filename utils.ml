open Core

let read_lines name =
  try In_channel.read_lines name
  with Sys_error msg -> failwith ("Failed to read from file: " ^ msg)

let read_string name =
  try In_channel.read_all name
  with Sys_error msg -> failwith ("Failed to read from file: " ^ msg)

let print_int = Printf.printf "%d\n"

let print_list lst =
  List.iter lst ~f:(printf "%d ");
  Out_channel.newline stdout

let list_contains lst x = List.mem lst x ~equal:( = )

(** Swap values u and v in list *)
let rec list_swap u v = function
  | [] -> []
  | x :: xs -> (if x = u then v else if x = v then u else x) :: list_swap u v xs

let rec find_index v = function
  | [] -> failwith "not found"
  | x :: xs -> if x = v then 0 else 1 + find_index v xs

let sum = List.reduce_exn ~f:(+)