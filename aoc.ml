open Core

let read_file name =
  try In_channel.read_lines name
  with Sys_error msg -> failwith ("Failed to read from file: " ^ msg)

let extract_digits line =
  String.split line ~on:' '
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:int_of_string
  |> function
  | [ x; y ] -> (x, y)
  | _ -> failwith "not 2 elements"

let () =
  let lines = read_file "input.txt" in
  let a, b = List.map ~f:extract_digits lines |> List.unzip in

  let map =
    List.fold b ~init:Int.Map.empty ~f:(fun map num ->
        Map.update map num ~f:(function None -> 1 | Some count -> count + 1))
  in

  List.map a ~f:(fun a -> a * Option.value (Map.find map a) ~default:0)
  |> List.reduce_exn ~f:( + ) |> string_of_int |> print_endline
