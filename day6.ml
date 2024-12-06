open Core
open Utils

type dir = North | East | South | West

let next_dir = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let next_pos pos dir =
  let x, y = pos in
  match dir with
  | North -> (x, y - 1)
  | East -> (x + 1, y)
  | South -> (x, y + 1)
  | West -> (x - 1, y)

let pos_equal (a1, b1) (a2, b2) = a1 = a2 && b1 = b2

let _print_arr arr =
  Array.iter arr ~f:(fun line ->
      Array.iter line ~f:(Printf.printf "%c");
      print_endline "")

let oob arr pos =
  let x, y = pos in
  x < 0 || y < 0 || x >= Array.length arr.(0) || y >= Array.length arr

let part1 input =
  let arr = List.map input ~f:String.to_array |> List.to_array in

  let guard_pos =
    let y = ref 0 in
    let x = ref 0 in
    Array.iteri arr ~f:(fun yy line ->
        match Array.findi line ~f:(fun _ c -> Char.equal '^' c) with
        | Some (xx, _) ->
            x := xx;
            y := yy
        | None -> ());
    (!x, !y)
  in

  let mark_pos (x, y) = arr.(y).(x) <- 'X' in

  let rec traverse pos dir =
    if oob arr pos then ()
    else (
      mark_pos pos;
      let next = next_pos pos dir in
      let x, y = next in

      if oob arr next then ()
      else if Char.equal arr.(y).(x) '#' then traverse pos (next_dir dir)
      else traverse next dir)
  in
  traverse guard_pos North;

  Array.map arr ~f:(fun l -> Array.count l ~f:(Char.equal 'X'))
  |> Array.reduce_exn ~f:( + ) |> print_int;
  ()

let part2 input =
  let arr = List.map input ~f:String.to_array |> List.to_array in

  let guard_pos =
    let y = ref 0 in
    let x = ref 0 in
    Array.iteri arr ~f:(fun yy line ->
        match Array.findi line ~f:(fun _ c -> Char.equal '^' c) with
        | Some (xx, _) ->
            x := xx;
            y := yy
        | None -> ());
    (!x, !y)
  in

  let dir_set = Hash_set.Poly.create () in

  let mark_pos pos =
    if pos_equal pos guard_pos then () else Hash_set.add dir_set pos
  in

  let rec traverse pos dir =
    if oob arr pos then ()
    else (
      mark_pos pos;
      let next = next_pos pos dir in
      let x, y = next in

      if oob arr next then ()
      else if Char.equal arr.(y).(x) '#' then traverse pos (next_dir dir)
      else traverse next dir)
  in
  traverse guard_pos North;

  let all_positions = Hash_set.to_list dir_set in

  let rec infinite obs_pos pos dir seen =
    if oob arr pos then false
    else
      let next = next_pos pos dir in
      let x, y = next in

      Hash_set.add seen (pos, dir);

      if oob arr next then false
      else if Hash_set.mem seen (next, dir) then true
      else if Char.equal arr.(y).(x) '#' || pos_equal obs_pos next then
        infinite obs_pos pos (next_dir dir) seen
      else infinite obs_pos next dir seen
  in

  List.count all_positions ~f:(fun pos ->
      infinite pos guard_pos North (Hash_set.Poly.create ()))
  |> print_int;

  ()

let () =
  let map = Utils.read_lines "input/day6.txt" in
  part1 map;
  part2 map
