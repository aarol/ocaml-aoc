open Core

let input = Utils.read_lines "input/day16.txt"
let grid = Utils.grid_from_input input

type dir = East | South | West | North

let forward = function
  | East -> (1, 0)
  | South -> (0, 1)
  | West -> (-1, 0)
  | North -> (0, -1)

let sides = function
  | East -> [ North; South ]
  | South -> [ East; West ]
  | West -> [ South; North ]
  | North -> [ East; West ]

let ( +^ ) (ax, ay) (bx, by) = (ax + bx, ay + by)
let ( =^ ) (ax, ay) (bx, by) = ax = bx && ay = by
let ( =& ) a b = equal_char a b
let ( @ ) grid (x, y) = grid.(y).(x)

exception Found_end of int

let find_path start_pos =
  let end_pos = Utils.find_in_grid grid 'E' in

  let scores = Pairing_heap.create ~cmp:Poly.compare () in
  Pairing_heap.add scores (0, (start_pos, East));

  let visited = Utils.TupleHashTbl.create () in

  try
    while not @@ Pairing_heap.is_empty scores do
      let score, (pos, dir) = Pairing_heap.pop_exn scores in
      if pos =^ end_pos then raise (Found_end score);

      let fwd = pos +^ forward dir in
      if not (grid @ fwd =& '#') then
        Hashtbl.update visited fwd ~f:(fun o ->
            let cw = Option.value o ~default:Int.max_value in
            if score + 1 <= cw then (
              Pairing_heap.add scores (score + 1, (fwd, dir));
              score + 1)
            else cw);
      sides dir
      |> List.iter ~f:(fun dir ->
             let fwd = pos +^ forward dir in
             if not (grid @ fwd =& '#') then
               Hashtbl.update visited fwd ~f:(fun o ->
                   let cw = Option.value o ~default:Int.max_value in
                   if score + 1001 < cw then (
                     Pairing_heap.add scores (score + 1001, (fwd, dir));
                     score + 1001)
                   else cw))
    done;
    0
  with Found_end score -> score

let part1 () =
  let start_pos = Utils.find_in_grid grid 'S' in
  Utils.print_int (find_path start_pos)

let path_exists at = find_path at > 0

let rec num_paths target =
  


let end_pos = Utils.find_in_grid grid 'E'

let part2 () =
  let start_pos = Utils.find_in_grid grid 'S' in
  let t_score = find_path start_pos in

  let scores = Pairing_heap.create ~cmp:Poly.compare () in
  Pairing_heap.add scores (0, (start_pos, East));

  let visited = Utils.TupleHashTbl.create () in

  let came_from = Utils.TupleHashTbl.create () in

  try
    while not @@ Pairing_heap.is_empty scores do
      let score, (pos, dir) = Pairing_heap.pop_exn scores in
      if pos =^ end_pos then raise (Found_end score);

      let fwd = pos +^ forward dir in
      if not (grid @ fwd =& '#') then
        Hashtbl.update visited fwd ~f:(fun o ->
            let cw = Option.value o ~default:Int.max_value in
            if score + 1 <= cw then (
              Hashtbl.set came_from ~key:fwd ~data:pos;
              Pairing_heap.add scores (score + 1, (fwd, dir));
              score + 1)
            else cw);
      sides dir
      |> List.iter ~f:(fun dir ->
             let fwd = pos +^ forward dir in
             if not (grid @ fwd =& '#') then
               Hashtbl.update visited fwd ~f:(fun o ->
                   let cw = Option.value o ~default:Int.max_value in
                   if score + 1001 < cw then (
                     Hashtbl.set came_from ~key:fwd ~data:pos;
                     Pairing_heap.add scores (score + 1001, (fwd, dir));
                     score + 1001)
                   else cw))
    done;
    failwith "Not found"
  with Found_end score ->
    Utils.print_int score;

    let rec path pos =
      if pos =^ start_pos then []
      else
        let prev = Hashtbl.find_exn came_from pos in
        prev :: path prev
    in
    List.iter (path end_pos) ~f:(fun (x, y) -> printf "%d, %d\n" x y);

    ()

let () =
  part1 ();
  part2 ()
