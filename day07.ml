open Core
open Utils

let solve input valid_fn =
  List.map input ~f:(fun line ->
      match String.split line ~on:':' with
      | [ x; y ] ->
          let target = int_of_string x in
          let numbers =
            Utils.string_split_whitespace y |> Utils.ints_of_strings
          in
          if valid_fn 0 target numbers then target else 0
      | _ -> failwith "not 2 elements")
  |> sum |> print_int

let part1 lines =
  let rec is_valid curr target = function
    | [] -> curr = target
    | x :: xs -> is_valid (curr + x) target xs || is_valid (curr * x) target xs
  in

  solve lines is_valid

let part2 lines =
  let concat x y = int_of_string (string_of_int x ^ string_of_int y) in

  let rec is_valid_concat curr target = function
    | [] -> curr = target
    | x :: xs ->
        is_valid_concat (curr + x) target xs
        || is_valid_concat (curr * x) target xs
        || is_valid_concat (concat curr x) target xs
  in

  solve lines is_valid_concat

let () =
  let lines = Utils.read_lines "input/day7.txt" in
  part1 lines;
  part2 lines
