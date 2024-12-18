open Core

let size = 71
let num_bytes = 1024
let input = Utils.read_lines "input/day18.txt"
let p1_input = List.take input num_bytes
let p2_input = List.drop input num_bytes
let grid = Array.(init size ~f:(fun _ -> create ~len:size '.'))
let ( =^ ) (ax, ay) (bx, by) = ax = bx && ay = by
let ( @ ) grid (x, y) = grid.(y).(x)
let ( =& ) a b = equal_char a b

(* Start every *)
let () =
  List.iter p1_input ~f:(fun l ->
      let x, y = Scanf.sscanf l "%d,%d" Tuple2.create in

      grid.(y).(x) <- '#')

exception FoundPath of int
exception NotFound

let find_path grid =
  let open Utils in
  let visited = TupleHashTbl.create () in
  let s = ref @@ Stack.create () in

  Stack.push !s (0, 0);

  try
    for i = 0 to Int.max_value do
      if Stack.is_empty !s then raise NotFound;

      let new_s = Stack.create () in

      Stack.until_empty !s (fun pos ->
          if pos =^ (size - 1, size - 1) then raise (FoundPath i);

          let neighbors = Utils.neighbours pos in
          List.iter neighbors ~f:(fun n ->
              if Utils.in_bounds grid n && grid @ n =& '.' then
                if not @@ Hashtbl.mem visited n then (
                  Stack.push new_s n;
                  Hashtbl.set visited ~key:n ~data:true)));

      s := new_s
    done;
    raise NotFound
  with FoundPath w -> w

let part1 () =
  let steps = find_path grid in
  Utils.print_int steps;

  ()

let part2 () =
  List.iter p2_input ~f:(fun line ->
      let x, y = Scanf.sscanf line "%d,%d" Tuple2.create in
      grid.(y).(x) <- '#';

      try
        let _ = find_path grid in
        ()
      with NotFound ->
        printf "Found: %s" line;
        exit 0);
  ()

let () =
  part1 ();
  part2 ()
