open Core

let digit_from_char c = int_of_char c - 48

type block_value = Free | Sized of int
type block = { len : int; value : block_value }

let to_block_list line =
  List.mapi line ~f:(fun i c ->
      if i % 2 = 0 then { len = digit_from_char c; value = Sized (i / 2) }
      else { len = digit_from_char c; value = Free })

let part2 line =
  let list = to_block_list line in

  let rec aux i = function
    | [] -> 0
    | x :: xs -> (
        Utils.print_int x.len;
        match x.value with
        | Sized size -> (i * size) + aux (i + 1) xs
        | Free -> (
            match List.last xs with
            | Some last -> (
                match last.value with
                | Sized size -> (i * size) + aux (i + 1) (List.drop_last_exn xs)
                | Free -> aux i (List.drop_last_exn (x :: xs)))
            | None -> 0))
  in

  aux 0 list |> Utils.print_int;

  ()

let part2 line =
  let list = to_block_list line in

     let rec aux i = function
       | [] -> 0
       | x :: xs -> (
           Utils.print_int i;
           if not (String.equal x ".") then (i * int_of_string x) + aux (i + 1) xs
           else
             match List.last xs with
             | Some last ->
                 if String.equal last "." then aux i (List.drop_last_exn (x :: xs))
                 else
                   (i * int_of_string last) + aux (i + 1) (List.drop_last_exn xs)
             | None -> 0)
     in

     aux 0 list |> Utils.print_int;

     ()
  ()

let () =
  let lines = Utils.read_string "input/day09.txt" |> String.to_list in

  part1 lines;
  part2 lines
