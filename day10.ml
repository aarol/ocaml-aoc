open Core

let neighbours (x, y) = [ (x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1) ]

let next_target = function
  | '0' -> '1'
  | '1' -> '2'
  | '2' -> '3'
  | '3' -> '4'
  | '4' -> '5'
  | '5' -> '6'
  | '6' -> '7'
  | '7' -> '8'
  | '8' -> '9'
  | d -> failwithf "invalid target: %c" d ()

let grid =
  Utils.read_lines "input/day10.txt"
  |> Array.of_list
  |> Array.map ~f:String.to_array

let grid_w = Array.length grid.(0)
let grid_h = Array.length grid

let collect trails_from =
  Array.foldi grid ~init:[] ~f:(fun y acc row ->
      acc
      @ Array.foldi row ~init:[] ~f:(fun x acc v ->
            acc @ if Char.equal v '0' then trails_from (x, y) else []))

let trails_from (x, y) =
  let rec aux target (x, y) =
    if x < 0 || x >= grid_w || y < 0 || y >= grid_h then []
    else
      let v = grid.(y).(x) in
      if Char.equal v '.' then []
      else if Char.equal v '9' then
        if Char.equal target '9' then [ (x, y) ] else []
      else if Char.equal v target then
        neighbours (x, y)
        |> List.fold ~init:[] ~f:(fun acc pos -> acc @ aux (next_target v) pos)
      else []
  in

  aux '0' (x, y)

let part1 () =
  let arr =
    collect (fun (x, y) ->
        trails_from (x, y) |> List.stable_dedup ~compare:Poly.compare)
  in

  arr |> List.length |> Utils.print_int

let part2 () = collect trails_from |> List.length |> Utils.print_int

let () =
  part1 ();
  part2 ()
