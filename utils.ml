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

let sum = List.reduce_exn ~f:( + )
let sum_array = Array.reduce_exn ~f:( + )

let string_split_whitespace str =
  String.split str ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)

let ints_of_strings = List.map ~f:int_of_string

let digit_of_char char = Char.to_int char - 48

let map_tuple f (a, b) = (f a, f b)
