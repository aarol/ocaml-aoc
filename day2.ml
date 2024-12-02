open Core

let process_line line =
  String.split line ~on:' '
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:int_of_string

let increasing line =
  List.fold_until (List.drop line 1) ~init:(List.nth_exn line 0)
    ~f:(fun last x ->
      if last < x && abs (x - last) <= 3 then Continue x else Stop false)
    ~finish:(fun _i -> true)

let decreasing line =
  List.fold_until (List.drop line 1) ~init:(List.nth_exn line 0)
    ~f:(fun last x ->
      if last > x && abs (last - x) <= 3 then Continue x else Stop false)
    ~finish:(fun _i -> true)

let part2 lines =
  List.filter lines ~f:(fun l ->
      List.init (List.length l) ~f:(fun n -> List.split_n l n)
      |> List.exists ~f:(fun (a, b) ->
             let list = a @ List.drop b 1 in
             increasing list || decreasing list))
  |> List.length |> string_of_int |> print_endline

let part1 lines =
  List.filter lines ~f:(fun l -> increasing l || decreasing l)
  |> List.length |> string_of_int |> print_endline

let () =
  let lines = Utils.read_file "input/day2.txt" |> List.map ~f:process_line in

  part1 lines;
  part2 lines
