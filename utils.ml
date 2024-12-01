open Core

let read_file name =
  try In_channel.read_lines name
  with Sys_error msg -> failwith ("Failed to read from file: " ^ msg)
