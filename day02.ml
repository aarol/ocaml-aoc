open Core

let process_line line =
  String.split line ~on:' '
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:int_of_string

let satisfies line op =
  let rec aux (last : int) = function
    | [] -> true
    | x :: rest when op last x && abs (last - x) <= 3 -> aux x rest
    | _ -> false
  in

  match line with x :: xs -> aux x xs | _ -> failwith "empty list"

let monotonic list = satisfies list ( < ) || satisfies list ( > )

let part2 lines =
  List.filter lines ~f:(fun l ->
      List.init (List.length l) ~f:(fun n -> List.split_n l n)
      |> List.exists ~f:(fun (a, b) -> monotonic (a @ List.drop b 1)))
  |> List.length |> string_of_int |> print_endline

let part1 lines =
  List.filter lines ~f:monotonic
  |> List.length |> string_of_int |> print_endline

let () =
  let lines = Utils.read_lines "input/day2.txt" |> List.map ~f:process_line in

  part1 lines;
  part2 lines
