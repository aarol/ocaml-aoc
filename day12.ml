open Core

let grid =
  Utils.read_lines "input/day12.txt"
  |> List.to_array
  |> Array.map ~f:String.to_array

let visited =
  Array.(init (length grid) ~f:(fun _ -> create ~len:(length grid.(0)) 0))

let in_bounds (x, y) = Utils.in_bounds grid (x, y)

let region_plots (x, y) =
  let rec aux (x, y) value =
    if not (in_bounds (x, y)) then []
    else
      match grid.(y).(x) with
      | v when equal_char value v && visited.(y).(x) = 0 ->
          visited.(y).(x) <- 1;
          (x, y)
          :: List.fold
               (Utils.neighbours (x, y))
               ~init:[]
               ~f:(fun acc (x, y) -> acc @ aux (x, y) v)
      | _ -> []
  in

  aux (x, y) grid.(y).(x)

let all_regions =
  Array.concat_mapi grid ~f:(fun y row ->
      Array.filter_mapi row ~f:(fun x _v ->
          if equal_char grid.(y).(x) '.' then None
          else
            let plots = region_plots (x, y) in
            let area = List.length plots in
            if area > 0 then Some plots else None))

let region_perimeter plots =
  let fences_around (x, y) =
    let value = grid.(y).(x) in
    List.count
      (Utils.neighbours (x, y))
      ~f:(fun (x, y) ->
        if in_bounds (x, y) then not @@ equal_char grid.(y).(x) value else true)
  in
  List.fold ~init:0 plots ~f:(fun acc (x, y) -> acc + fences_around (x, y))

let part1 =
  Array.fold ~init:0 all_regions ~f:(fun acc plots ->
      acc + (region_perimeter plots * List.length plots))
  |> Utils.print_int

let same_region (x, y) plots =
  if Utils.out_of_bounds grid (x, y) then false
  else List.mem plots (x, y) ~equal:Poly.equal

let ( -^ ) (ax, ay) (bx, by) = (ax - bx, ay - by)
let ( +^ ) (ax, ay) (bx, by) = (ax + bx, ay + by)

let sliding_window lst =
  let tail = List.tl_exn lst @ [ List.hd_exn lst ] in
  List.map2_exn lst tail ~f:(fun x y -> (x, y))

let region_sides plots =
  List.fold ~init:0 plots ~f:(fun acc pos ->
      let corners =
        Utils.neighbours pos |> sliding_window
        |> List.count ~f:(fun (npos, next) ->
               if same_region npos plots then
                 (* neighbor is same *)
                 if same_region next plots then
                   (* next clockwise is same *)
                   let n_dv = npos -^ pos in
                   let next_dv = next -^ pos in
                   let diagonal = pos +^ n_dv +^ next_dv in
                   not (same_region diagonal plots)
                 else (* next clockwise is not same *)
                   false
               else (* neighbor is not same *)
                 not (same_region next plots))
      in
      acc + corners)

let part2 =
  Array.fold ~init:0 all_regions ~f:(fun acc plots ->
      acc + (region_sides plots * List.length plots))
  |> Utils.print_int

let () =
  part1;
  part2
