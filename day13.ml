open Core

type offset = int * int

(* let ( -^ ) (ax, ay) (bx, by) = (ax - bx, ay - by)
   let ( =^ ) (ax, ay) (bx, by) = ax = bx && ay = by
   let ( %^ ) (ax, ay) (bx, by) = ax mod bx = 0 && ay mod by = 0
   let ( /^ ) (ax, _ay) (bx, _by) = ax / bx *)
(* let _negative (x, y) = x < 0 || y < 0 *)

type machine = { a : offset; b : offset; prize : offset }

let machines prize_add =
  Utils.read_lines "input/day13.txt"
  |> List.chunks_of ~length:4
  |> List.map ~f:(fun lines ->
         match lines with
         | [ l1; l2; l3; _ ] | [ l1; l2; l3 ] ->
             let a = Scanf.sscanf l1 "Button A: X+%d, Y+%d" Tuple2.create in
             let b = Scanf.sscanf l2 "Button B: X+%d, Y+%d" Tuple2.create in
             let prize =
               Scanf.sscanf l3 "Prize: X=%d, Y=%d" (fun a b ->
                   (a + prize_add, b + prize_add))
             in
             { a; b; prize }
         | a -> failwithf "not 4 lines: %d" (List.length a) ())

let minimum a b target =
  let c1, c2 = target in
  let a1, a2 = a in
  let b1, b2 = b in

  let x = ((c1 * b2) - (b1 * c2)) / ((a1 * b2) - (b1 * a2)) in
  let y = ((a1 * c2) - (c1 * a2)) / ((a1 * b2) - (b1 * a2)) in

  if (a1 * x) + (b1 * y) = c1 && (a2 * x) + (b2 * y) = c2 then (3 * x) + y
  else 0

let minimum_tokens { a; b; prize } = minimum a b prize

let part1 () =
  List.fold ~init:0 (machines 0) ~f:(fun acc m ->
      let res = minimum_tokens m in

      (* Printf.printf "res: %d\n" res; *)
      acc + res)
  |> Utils.print_int

let part2 () =
  List.fold ~init:0 (machines 10000000000000) ~f:(fun acc m ->
      let res = minimum_tokens m in

      (* Printf.printf "res: %d\n" res; *)
      acc + res)
  |> Utils.print_int

let () =
  part1 ();
  part2 ()
