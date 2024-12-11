open Core

let digit_from_char c = int_of_char c - 48

type block = { len : int; value : int }
type free = { len : int }

let to_str_list line =
  Array.concat_mapi line ~f:(fun i c ->
      if i % 2 = 0 then
        Array.init (digit_from_char c) ~f:(fun _ -> string_of_int (i / 2))
      else Array.init (digit_from_char c) ~f:(fun _ -> "."))

let spaces line =
  let acc = ref 0 in
  Array.filter_mapi line ~f:(fun i c ->
      let len = digit_from_char c in
      let curr_acc = !acc in
      acc := !acc + len;
      if i % 2 = 1 then Some (curr_acc, { len }) else None)

let blocks line =
  let acc = ref 0 in
  Array.filter_mapi line ~f:(fun i c ->
      let len = digit_from_char c in
      let cacc = !acc in
      acc := !acc + len;
      if i % 2 = 0 then Some (cacc, { value = i / 2; len }) else None)

let part1 line =
  let arr = to_str_list line in

  let rec aux i last_i arr =
    if last_i < i then 0
    else
      let x = arr.(i) in
      if not @@ String.equal x "." then
        (int_of_string x * i) + aux (i + 1) last_i arr
      else
        (* "." at i *)
        let last = arr.(last_i) in
        if String.equal last "." then aux i (last_i - 1) arr
        else (int_of_string last * i) + aux (i + 1) (last_i - 1) arr
  in

  aux 0 (Array.length arr - 1) arr |> Utils.print_int;

  ()

let part2 line =
  let blocks = Array.rev (blocks line) in
  let spaces = spaces line in

  

  let rec sum_of_block value size i =
    if size <= 0 then 0 else (value * i) + sum_of_block value (size - 1) (i + 1)
  in

  Array.fold blocks ~init:0 ~f:(fun acc (b_i, b) ->
      let found =
        spaces
        |> Array.findi ~f:(fun _ (f_i, f) -> f_i <= b_i && f.len >= b.len)
      in
      acc
      +
      match found with
      | Some (i, (f_i, f)) ->
          spaces.(i) <- (f_i + b.len, { len = f.len - b.len });
          let sum = sum_of_block b.value b.len f_i in
          printf "%d\n" sum;
          sum
      | None -> sum_of_block b.value b.len b_i)
  |> Utils.print_int;

  ()

let () =
  let lines = Utils.read_string "input/day09.txt" |> String.to_array in

  part1 lines;
  part2 lines
