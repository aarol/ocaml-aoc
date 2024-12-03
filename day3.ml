open Core

let digit_regex =
  Re.compile
    Re.(
      seq [ str "mul("; group (rep digit); str ","; group (rep digit); str ")" ])

let do_regex = Re.compile Re.(str "do()")
let don't_regex = Re.compile Re.(str "don't()")

let part2 line =
  let do_matches = Re.all do_regex line in
  let don't_matches = Re.all don't_regex line in

  let don't_ranges =
    don't_matches
    |> List.map ~f:(fun m -> Tuple.T2.get2 (Re.Group.offset m 0) + 1)
  in
  let do_ranges =
    do_matches |> List.map ~f:(fun m -> Tuple.T2.get2 (Re.Group.offset m 0) + 1)
  in

  let rec valid_idx = function
    | 0 -> true
    | x when List.mem don't_ranges x ~equal:Int.equal -> false
    | x when List.mem do_ranges x ~equal:Int.equal -> true
    | x -> valid_idx (x - 1)
  in

  let matches = Re.all digit_regex line in

  let match_to_int a b = int_of_string (String.sub line ~pos:a ~len:(b - a)) in

  List.fold matches ~init:0 ~f:(fun acc group ->
      match Re.Group.all_offset group with
      | [| _; (a, b); (c, d) |] ->
          acc + if valid_idx a then match_to_int a b * match_to_int c d else 0
      | _ -> failwith "not 2 captures")
  |> string_of_int |> print_endline

let part1 line =
  let matches = Re.all digit_regex line in

  List.fold matches ~init:0 ~f:(fun acc group ->
      match Re.Group.all group with
      | [| _; x; y |] -> acc + (int_of_string x * int_of_string y)
      | _ ->
          failwith
            ("not 2 captures"
            ^ string_of_int (Array.length (Re.Group.all group))))
  |> string_of_int |> print_endline

let () =
  let line = Utils.read_string "input/day3.txt" in

  part1 line;
  part2 line
