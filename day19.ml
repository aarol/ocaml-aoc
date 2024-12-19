open Core

let input = Utils.read_lines "input/day19.txt"

let patterns =
  List.hd_exn input
  |> String.split_on_chars ~on:[ ','; ' ' ]
  |> List.filter ~f:(Fn.non String.is_empty)

let designs = List.drop input 2

let rec is_possible design =
  String.is_empty design
  || patterns
     |> List.filter_map ~f:(fun pattern ->
            String.chop_prefix design ~prefix:pattern)
     |> List.exists ~f:(fun substr -> is_possible substr)

let part1 () =
  List.count designs ~f:(fun line -> is_possible line) |> Utils.print_int

let memo = Hashtbl.create (module String)

let rec possible_ways design =
  if String.is_empty design then 1
  else
    Hashtbl.find_or_add memo design ~default:(fun () ->
        patterns
        |> List.filter_map ~f:(fun pattern ->
               String.chop_prefix design ~prefix:pattern)
        |> List.fold ~init:0 ~f:(fun acc substr -> acc + possible_ways substr))

let part2 () =
  List.fold ~init:0 designs ~f:(fun acc line -> acc + possible_ways line)
  |> Utils.print_int

let () =
  part1 ();
  part2 ()
