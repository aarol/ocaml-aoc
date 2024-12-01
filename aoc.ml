open Core

let digits =
  [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
  ]
  |> List.map ~f:(fun (s, x) -> (String.to_list s, x))

let read_file name =
  try In_channel.read_lines name
  with Sys_error msg -> failwith ("Failed to read from file: " ^ msg)

let digits_in_string s =
  let rec f chars =
    match chars with
    | [] -> []
    | c :: rest when Char.is_digit c ->
        let digit = String.of_char c |> Int.of_string in
        digit :: f rest
    | _ :: rest -> (
        match
          List.find digits ~f:(fun (word, _) ->
              List.is_prefix chars ~prefix:word ~equal:Char.equal)
        with
        | Some (_, digit) -> digit :: f rest
        | None -> f rest)
  in
  f (String.to_list s)

let extract_digits line =
  let chars = String.to_list line in
  let a = List.find_exn ~f:Char.is_digit chars in
  let b = List.find_exn ~f:Char.is_digit (List.rev chars) in
  int_of_string (Char.to_string a ^ Char.to_string b)

let () =
  let lines = read_file "input.txt" in
  let nums = List.map ~f:extract_digits lines in
  let sum = List.reduce_exn ~f:( + ) nums in
  string_of_int sum |> print_endline
