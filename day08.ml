open Core

let all_pairs lst =
  List.concat_map lst ~f:(fun (ax, ay) ->
      List.filter_map lst ~f:(fun (bx, by) ->
          if ax <> bx && ay <> by then Some ((ax, ay), (bx, by)) else None))

let _distance (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)
let vector (ax, ay) (bx, by) = (ax - bx, ay - by)
let ( *^ ) (x, y) mul = (x * mul, y * mul)
let ( +^ ) (ax, ay) (bx, by) = (ax + bx, ay + by)

let in_bounds (x, y) grid =
  x >= 0 && y >= 0 && x < Array.length grid.(0) && y < Array.length grid

let part1 grid =
  let all_antennas =
    Array.foldi grid ~init:Map.Poly.empty ~f:(fun y map row ->
        Array.foldi row ~init:map ~f:(fun x map cell ->
            if not (Char.equal cell '.') then
              Map.add_multi map ~key:cell ~data:(x, y)
            else map))
  in

  let antinodes =
    Array.init (Array.length grid) ~f:(fun _ ->
        Array.init (Array.length grid.(0)) ~f:(fun _ -> 0))
  in

  Map.iter_keys all_antennas ~f:(fun key ->
      let antennas = Map.find_multi all_antennas key in
      let perms = all_pairs antennas in
      List.iter perms ~f:(fun (a, b) ->
          let v = vector a b in
          let an_pos = b +^ (v *^ 2) in

          if in_bounds an_pos grid then
            let x, y = an_pos in
            antinodes.(y).(x) <- 1);
      ());

  Array.map antinodes ~f:Utils.sum_array |> Utils.sum_array |> Utils.print_int;
  ()

let part2 grid =
  let all_antennas =
    Array.foldi grid ~init:Map.Poly.empty ~f:(fun y map row ->
        Array.foldi row ~init:map ~f:(fun x map cell ->
            if not (Char.equal cell '.') then
              Map.add_multi map ~key:cell ~data:(x, y)
            else map))
  in

  let antinodes =
    Array.init (Array.length grid) ~f:(fun _ ->
        Array.init (Array.length grid.(0)) ~f:(fun _ -> 0))
  in

  Map.iter_keys all_antennas ~f:(fun key ->
      let antennas = Map.find_multi all_antennas key in
      let perms = all_pairs antennas in
      List.iter perms ~f:(fun (a, b) ->
          let v = vector a b in

          let pos = ref (b +^ v) in
          while in_bounds !pos grid do
            let x, y = !pos in
            antinodes.(y).(x) <- 1;
            pos := !pos +^ v
          done));

  Array.map antinodes ~f:Utils.sum_array |> Utils.sum_array |> Utils.print_int;
  ()

let () =
  let lines = Utils.read_lines "input/day08.txt" in

  let grid = Array.map (List.to_array lines) ~f:String.to_array in

  part1 grid;
  part2 grid
