open Core

let rec horizontal = function
  | 'X' :: 'M' :: 'A' :: 'S' :: rest -> 1 + horizontal rest
  | [] -> 0
  | _ :: rest -> horizontal rest

let horizontal_all lst = horizontal lst + horizontal (List.rev lst)

let vertical lst_lst =
  List.transpose_exn lst_lst |> List.map ~f:horizontal_all
  |> List.reduce_exn ~f:( + )

let matrix_at matrix x y = List.nth_exn (List.nth_exn matrix x) y

let diagonal matrix =
  let length = List.length matrix in

  let rec diagonal_from x y acc =
    if x >= length || y >= length then acc
    else diagonal_from (x + 1) (y + 1) (matrix_at matrix x y :: acc)
  in

  let total = ref 0 in

  for start_col = 0 to length do
    total := !total + horizontal_all (diagonal_from 0 start_col [])
  done;
  for start_row = 1 to length do
    total := !total + horizontal_all (diagonal_from start_row 0 [])
  done;

  !total

let part1 lines =
  let chars = List.map lines ~f:String.to_list in
  let total =
    (chars |> List.map ~f:horizontal_all |> List.reduce_exn ~f:( + ))
    + vertical chars + diagonal chars
    + (List.rev chars |> diagonal)
  in

  Utils.print_int total;
  ()

let part2 lines =
  let chars = List.map lines ~f:String.to_list in

  let at_index x y = List.nth_exn (List.nth_exn chars y) x in

  let mas lst =
    let rec aux = function
      | [] -> false
      | 'M' :: 'A' :: 'S' :: _ -> true
      | _ :: rest -> aux rest
    in
    aux lst || aux (List.rev lst)
  in

  let length = List.length chars in
  List.mapi chars ~f:(fun y row ->
      if y < length - 2 then
        List.mapi row ~f:(fun x v ->
            if x < length - 2 then
              if
                mas [ v; at_index (x + 1) (y + 1); at_index (x + 2) (y + 2) ]
                && mas
                     [
                       at_index (x + 2) y;
                       at_index (x + 1) (y + 1);
                       at_index x (y + 2);
                     ]
              then 1
              else 0
            else 0)
        |> List.reduce_exn ~f:( + )
      else 0)
  |> List.reduce_exn ~f:( + ) |> Utils.print_int;

  ()

let () =
  let lines = Utils.read_lines "input/day4.txt" in

  part1 lines;
  part2 lines
