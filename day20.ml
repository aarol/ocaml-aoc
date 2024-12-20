open Core

let ( @ ) grid (x, y) = grid.(y).(x)
let ( =& ) a b = equal_char a b
let ( =^ ) (ax, ay) (bx, by) = ax = bx && ay = by
let ( *^ ) (a, b) m = (a * m, b * m)

(* type dir = North | East | South | West

   let next_in_dir = function
     | North -> [ (-1, 0); (0, -1); (1, 0) ]
     | East -> [ (0, 1); (0, -1); (1, 0) ]
     | South -> [ (-1, 0); (0, -1); (1, 0) ]
     | West -> [ (-1, 0); (0, -1); (0, 1) ] *)

let ( +^ ) (ax, ay) (bx, by) = (ax + bx, ay + by)
let grid = Utils.read_lines "input/day20.txt" |> Utils.grid_from_input
let distance (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

let within_20_distance =
  let lst = ref [] in
  for y = -20 to 20 do
    for x = -20 to 20 do
      if abs x + abs y <= 20 && not (x = 0 && y = 0) then lst := (x, y) :: !lst
    done
  done;
  !lst

let score_arr =
  Array.(init (length grid) ~f:(fun _ -> create ~len:(length grid.(0)) (-1)))

let start_pos = Utils.find_in_grid grid 'S'
let end_pos = Utils.find_in_grid grid 'E'

let next_pos prev curr =
  Utils.neighbours curr
  |> List.find_exn ~f:(fun n_pos ->
         (not (n_pos =^ prev))
         && Utils.in_bounds grid n_pos
         && not (grid @ n_pos =& '#'))

let rec walk_path prev curr =
  if curr =^ end_pos then (
    let x, y = end_pos in
    score_arr.(y).(x) <- 0;
    0)
  else
    let next_pos = next_pos prev curr in
    let score = walk_path curr next_pos in
    let x, y = curr in
    score_arr.(y).(x) <- score + 1;
    score + 1

let _ = walk_path (0, 0) start_pos

let count_valid_cheats cheat_fn =
  let count = ref 0 in

  let rec walk prev curr =
    if curr =^ end_pos then ()
    else (
      count := !count + cheat_fn curr;
      let next_pos = next_pos prev curr in
      walk curr next_pos)
  in

  walk (0, 0) start_pos;

  !count

let part1 min_saves =
  let cheat_fn curr =
    Utils.neighbours (0, 0)
    |> List.map ~f:(fun v -> curr +^ (v *^ 2))
    |> List.filter ~f:(fun p -> Utils.in_bounds grid p)
    |> List.count ~f:(fun p ->
           match score_arr @ p with
           | -1 -> false
           | s -> (score_arr @ curr) - s - 2 >= min_saves)
  in
  count_valid_cheats cheat_fn |> Utils.print_int

let part2 min_saves =
  let cheat_fn curr =
    within_20_distance
    |> List.map ~f:(fun v -> curr +^ v)
    |> List.filter ~f:(fun p -> Utils.in_bounds grid p)
    |> List.filter_map ~f:(fun p ->
           match score_arr @ p with
           | -1 -> None
           | s -> Some (s + distance p curr))
    |> List.count ~f:(fun s ->
           (* s is distance to the end at cheated point *)
           (score_arr @ curr) - s >= min_saves)
  in

  count_valid_cheats cheat_fn |> Utils.print_int

let () =
  part1 100;
  part2 100
