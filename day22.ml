open Core

let input = Utils.read_lines "input/day22.txt" |> Utils.ints_of_strings
let mix = Int.bit_xor
let prune n = n mod 16777216

let next_secret num =
  let num = prune (mix num (num * 64)) in
  let num = prune (mix num (num / 32)) in
  prune (mix num (num * 2048))

let calc_nth_secret times num = Fn.apply_n_times ~n:times next_secret num

let part1 () =
  List.sum (module Int) input ~f:(calc_nth_secret 2000) |> Utils.print_int

let sequence_scores = String.Table.create ()

let sequence_key list =
  List.fold list ~init:"" ~f:(fun acc num -> acc ^ "," ^ string_of_int num)

let iter_secrets num =
  let visited = Hash_set.create (module String) in

  let curr = ref (next_secret num) in
  let last_digit n = n mod 10 in
  let changes = ref [] in

  for _ = 0 to 3 do
    let next = next_secret !curr in
    let delta = last_digit next - last_digit !curr in
    changes := delta :: !changes;
    curr := next
  done;

  for _ = 4 to 2000 do
    let next = next_secret !curr in
    let delta = last_digit next - last_digit !curr in
    changes := delta :: List.drop_last_exn !changes;
    let key = sequence_key !changes in
    if not @@ Hash_set.mem visited key then (
      Hash_set.add visited key;
      Hashtbl.update sequence_scores key ~f:(fun o ->
          match o with None -> last_digit next | Some s -> s + last_digit next));
    curr := next;
    ()
  done

let part2 () =
  List.iter input ~f:iter_secrets;

  match
    List.max_elt (Hashtbl.to_alist sequence_scores)
      ~compare:(fun (_, v1) (_, v2) -> v1 - v2)
  with
  | None -> failwith "no max elt"
  | Some (k, v) -> printf "found %s: %d" (String.rev k) v

let () =
  part1 ();
  part2 ()
