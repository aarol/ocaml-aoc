open Core

let process_line line =
  String.split line ~on:' '
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:int_of_string
  |> function
  | [ x; y ] -> (x, y)
  | _ -> failwith "not 2 elements"

let part2 () =
  let lines = Utils.read_lines "input/day1.txt" in
  let a, b = List.map ~f:process_line lines |> List.unzip in

  let map =
    List.fold b ~init:Int.Map.empty ~f:(fun map num ->
        Map.update map num ~f:(function None -> 1 | Some count -> count + 1))
  in

  List.map a ~f:(fun a -> a * Option.value (Map.find map a) ~default:0)
  |> List.reduce_exn ~f:( + ) |> string_of_int |> print_endline

let part1 () =
  let lines = Utils.read_lines "input/day1.txt" in
  let a, b = List.map ~f:process_line lines |> List.unzip in

  List.fold2_exn (List.sort a ~compare:Int.compare)
    (List.sort b ~compare:Int.compare) ~init:0 ~f:(fun acc a b ->
      abs (a - b) + acc)
  |> Int.to_string |> print_endline

let () =
  part1 ();
  part2 ()
