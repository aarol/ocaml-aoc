open Core
open Utils

let parse_ab conditions =
  List.map conditions ~f:(fun c -> String.lsplit2_exn c ~on:'|')
  |> List.map ~f:(fun (a, b) -> (int_of_string a, int_of_string b))
  |> List.unzip

let part1 conditions pages =
  let a, b = parse_ab conditions in

  let m =
    List.foldi b ~init:Int.Map.empty ~f:(fun i map b ->
        Map.add_multi map ~key:b ~data:(List.nth_exn a i))
  in
  List.map pages ~f:(fun page ->
      let nums = String.split page ~on:',' |> List.map ~f:int_of_string in

      if
        (* For all numbers in page *)
        List.for_alli nums ~f:(fun i num ->
            (* all_before = numbers that should be before num *)
            let all_before = Map.find_multi m num in
            (* Numbers after current index *)
            let _, after_i = List.split_n nums i in
            (* For every num that should come before *)
            List.for_all all_before ~f:(fun b ->
                (* Numbers after current index should not contain numbers in all_before *)
                not (list_contains after_i b)))
      then List.nth_exn nums (List.length nums / 2)
      else 0)
  |> sum |> print_int

let part2 conditions pages =
  let a, b = parse_ab conditions in

  let m =
    List.foldi b ~init:Int.Map.empty ~f:(fun i m b ->
        Map.add_multi m ~key:b ~data:(List.nth_exn a i))
  in

  let pages =
    List.map pages ~f:(fun p ->
        String.split p ~on:',' |> List.map ~f:int_of_string)
  in

  let invalid =
    List.filter pages ~f:(fun page ->
        (* There is a number in page *)
        List.existsi page ~f:(fun i num ->
            (* all_before = numbers that should be before num *)
            let all_before = Map.find_multi m num in
            (* Numbers after current index *)
            let _, after_i = List.split_n page i in
            (* For which there is a number which should come after num *)
            List.exists all_before ~f:(fun b ->
                (* Numbers after current index contain numbers in all_before *)
                list_contains after_i b)))
  in

  let rec fix = function
    | [] -> []
    | x :: xs -> (
        (* Numbers that should come after x *)
        let after_x = Map.find_multi m x in
        (* If there is a number that should be after x *)
        match List.find after_x ~f:(fun ax -> Utils.list_contains xs ax) with
        (* Swap their places *)
        | Some s -> fix (Utils.list_swap s x (x :: xs))
        | None -> x :: fix xs)
  in

  List.map invalid ~f:(fun lst -> fix (List.rev lst))
  |> List.map ~f:(fun lst -> List.nth_exn lst (List.length lst / 2))
  |> sum |> print_int

let () =
  let conditions = Utils.read_lines "input/day5_a.txt" in
  let pages = Utils.read_lines "input/day5_b.txt" in

  part1 conditions pages;
  part2 conditions pages
