open Core

let grid_text = Utils.read_string "input/day15_a.txt"

let grid =
  grid_text |> String.split_lines |> Array.of_list
  |> Array.map ~f:String.to_array

let ins =
  Utils.read_string "input/day15_b.txt"
  |> String.substr_replace_all ~pattern:"\n" ~with_:""
  |> String.to_list

let ( @ ) grid (x, y) = grid.(y).(x)
let ( +^ ) (ax, ay) (bx, by) = (ax + bx, ay + by)
let ( =^ ) (ax, ay) (bx, by) = ax = bx && ay = by
let ( =& ) a b = equal_char a b

let part1 () =
  let pos = ref @@ Utils.find_in_grid grid '@' in

  let rec move_dir prev dir =
    let swap (ax, ay) (bx, by) =
      grid.(by).(bx) <- grid.(ay).(ax);
      grid.(ay).(ax) <- '.'
    in

    let new_pos = prev +^ dir in
    if grid @ new_pos =& '.' then (
      swap prev new_pos;
      if prev =^ !pos then pos := new_pos)
    else if grid @ new_pos =& 'O' then (
      move_dir new_pos dir;
      if grid @ new_pos =& '.' then move_dir prev dir else ())
  in

  List.iter ins ~f:(fun dir ->
      match dir with
      | '<' -> move_dir !pos (-1, 0)
      | '>' -> move_dir !pos (1, 0)
      | 'v' -> move_dir !pos (0, 1)
      | '^' -> move_dir !pos (0, -1)
      | '\n' -> ()
      | d -> failwith ("invalid dir: " ^ String.of_char d));

  Array.foldi grid ~init:0 ~f:(fun y acc row ->
      acc
      + Array.foldi row ~init:0 ~f:(fun x acc v ->
            acc + if equal_char v 'O' then (100 * y) + x else 0))
  |> Utils.print_int;
  ()

let grid =
  grid_text |> String.split_lines |> Array.of_list
  |> Array.map ~f:(fun line ->
         line
         |> String.substr_replace_all ~pattern:"#" ~with_:"##"
         |> String.substr_replace_all ~pattern:"." ~with_:".."
         |> String.substr_replace_all ~pattern:"O" ~with_:"[]"
         |> String.substr_replace_all ~pattern:"@" ~with_:"@."
         |> String.to_array)

let part2 () =
  let rec can_move prev dir =
    let next = prev +^ dir in
    match grid @ next with
    | '#' -> false
    | '.' -> true
    | '[' -> can_move next dir && can_move (next +^ (1, 0)) dir
    | ']' -> can_move next dir && can_move (next +^ (-1, 0)) dir
    | _ -> failwith "invalid grid"
  in

  let rec move_box prev dir =
    let swap (ax, ay) (bx, by) =
      grid.(by).(bx) <- grid.(ay).(ax);
      grid.(ay).(ax) <- '.'
    in

    match dir with
    | 1, 0 | -1, 0 ->
        let next = prev +^ dir +^ dir in
        let next_v = grid @ next in
        if next_v =& '.' then (
          swap (prev +^ dir) next;
          swap prev (prev +^ dir))
        else if next_v =& '[' || next_v =& ']' then (
          move_box next dir;
          if grid @ next =& '.' then move_box prev dir)
    | 0, -1 | 0, 1 -> (
        if can_move prev dir then
          let next = prev +^ dir in
          match grid @ next with
          | '.' -> (
              let prev_v = grid @ prev in
              swap prev next;
              match prev_v with
              | '[' ->
                  if grid @ (prev +^ (1, 0)) =& ']' then
                    move_box (prev +^ (1, 0)) dir
              | ']' ->
                  if grid @ (prev +^ (-1, 0)) =& '[' then
                    move_box (prev +^ (-1, 0)) dir
              | _ -> ())
          | '[' | ']' ->
              move_box next dir;
              if grid @ next =& '.' then move_box prev dir
          | _ -> ())
    | _ -> failwith "invalid dir"
  in

  let pos = ref @@ Utils.find_in_grid grid '@' in

  let rec move_dir prev dir =
    let swap (ax, ay) (bx, by) =
      grid.(by).(bx) <- grid.(ay).(ax);
      grid.(ay).(ax) <- '.'
    in

    let new_pos = prev +^ dir in

    match dir with
    (* horizontal *)
    | 1, 0 | -1, 0 -> (
        let v = grid @ new_pos in
        match v with
        | ']' | '[' ->
            move_box new_pos dir;
            if grid @ new_pos =& '.' then move_dir prev dir
        | '.' ->
            swap prev new_pos;
            pos := new_pos
        | _ -> ()
        (* vertical *))
    | 0, -1 | 0, 1 -> (
        let v = grid @ new_pos in
        match v with
        | '.' ->
            swap prev new_pos;
            pos := new_pos
        | ']' | '[' ->
            if can_move prev dir then move_box new_pos dir;
            if grid @ new_pos =& '.' then move_dir prev dir
        | _ -> ())
    | _ -> failwith "invalid dir"
  in

  List.iter ins ~f:(fun dir ->
      match dir with
      | '<' -> move_dir !pos (-1, 0)
      | '>' -> move_dir !pos (1, 0)
      | 'v' -> move_dir !pos (0, 1)
      | '^' -> move_dir !pos (0, -1)
      | '\n' -> ()
      | d -> failwith ("invalid dir: " ^ String.of_char d));

  Utils.print_grid grid;

  Array.foldi grid ~init:0 ~f:(fun y acc row ->
      acc
      + Array.foldi row ~init:0 ~f:(fun x acc v ->
            acc + if v =& '[' then (100 * y) + x else 0))
  |> Utils.print_int;
  ()

let () =
  part1 ();
  part2 ()
