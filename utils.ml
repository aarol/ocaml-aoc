open Core

let read_lines name =
  try In_channel.read_lines name
  with Sys_error msg -> failwith ("Failed to read from file: " ^ msg)

let read_string name =
  try In_channel.read_all name
  with Sys_error msg -> failwith ("Failed to read from file: " ^ msg)

let print_int = Printf.printf "%d\n"