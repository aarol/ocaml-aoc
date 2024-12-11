open Core

let input_line =
  Utils.read_string "input/day11.txt"
  |> String.split ~on:' ' |> List.map ~f:int_of_string

let split_digits num =
  let str = string_of_int num |> String.to_list in
  (* printf "%s\n" (String.of_list str); *)
  List.split_n str (List.length str / 2)
  |> Utils.map_tuple (fun s -> int_of_string @@ String.of_list s)

let solve iterations =
  let map = ref @@ Hashtbl.create (module Int) in

  let update map key value =
    Hashtbl.update map key ~f:(fun o -> Option.value o ~default:0 + value)
  in

  List.iter input_line ~f:(fun i -> update !map i 1);

  for _ = 1 to iterations do
    let new_map = Hashtbl.create (module Int) in
    Hashtbl.iteri !map ~f:(fun ~key ~data ->
        match key with
        | 0 -> update new_map 1 data
        | x when String.length (string_of_int x) % 2 = 0 ->
            let a, b = split_digits x in
            update new_map a data;
            update new_map b data
        | other -> update new_map (other * 2024) data);
    map := new_map
  done;

  Hashtbl.fold !map ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)
  |> Utils.print_int;
  ()

let () =
  solve 25;
  solve 75
