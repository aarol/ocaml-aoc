open Core
open Utils

let reduce input solve_fn =
  List.map input ~f:(fun line ->
      match String.split line ~on:':' with
      | [ x; y ] ->
          let target = int_of_string x in
          let numbers =
            Utils.string_split_whitespace y |> List.map ~f:int_of_string
          in
          if solve_fn 0 target numbers then target else 0
      | _ -> failwith "not 2 elements")
  |> sum |> print_int

let part1 lines =
  let rec solve curr target = function
    | [] -> curr = target
    | x :: xs -> solve (curr + x) target xs || solve (curr * x) target xs
  in

  reduce lines solve

let part2 lines =
  let concat x y = int_of_string (string_of_int x ^ string_of_int y) in

  let rec solve curr target lst =
    match lst with
    | [] -> curr = target
    | x :: xs ->
        solve (curr + x) target xs
        || solve (curr * x) target xs
        || solve (concat curr x) target xs
  in

  reduce lines solve

let () =
  let lines = Utils.read_lines "input/day7.txt" in
  part1 lines;
  part2 lines
