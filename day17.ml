open Core

let input = Utils.read_lines "input/day17.txt"

let a, b, c, program =
  match input with
  | [ a; b; c; _; d ] ->
      ( Scanf.sscanf a "Register A: %d" Fn.id,
        Scanf.sscanf b "Register B: %d" Fn.id,
        Scanf.sscanf c "Register C: %d" Fn.id,
        Scanf.sscanf d "Program: %s" Fn.id )
  | _ -> failwith "invalid input"

let ins =
  String.split program ~on:',' |> List.to_array |> Array.map ~f:int_of_string

let exec a_register =
  let a = ref a_register in
  let b = ref b in
  let c = ref c in

  let get_combo = function
    | x when x <= 3 -> x
    | 4 -> !a
    | 5 -> !b
    | 6 -> !c
    | _ -> failwith "invalid combo target"
  in

  let output = ref [] in

  let ip = ref 0 in

  let operand () = ins.(!ip + 1) in

  while !ip < Array.length ins do
    match ins.(!ip) with
    | 0 ->
        let op = operand () in
        a := !a / Int.pow 2 (get_combo op);
        ip := !ip + 2
    | 1 ->
        let op = operand () in

        b := Int.bit_xor !b op;
        ip := !ip + 2
    | 2 ->
        let op = operand () in
        b := get_combo op mod 8;
        ip := !ip + 2
    | 3 ->
        if !a <> 0 then
          let op = operand () in
          ip := op
        else ip := !ip + 2
    | 4 ->
        b := Int.bit_xor !b !c;
        ip := !ip + 2
    | 5 ->
        let op = operand () in
        let value = get_combo op mod 8 in
        output := value :: !output;
        ip := !ip + 2
    | 6 ->
        let op = operand () in
        b := !a / Int.pow 2 (get_combo op);
        ip := !ip + 2
    | 7 ->
        let op = operand () in
        c := !a / Int.pow 2 (get_combo op);
        ip := !ip + 2
    | _ -> failwith "invalid opcode"
  done;

  printf "a: %d\n" !a;
  printf "b: %d\n" !b;
  printf "c: %d\n" !c;
  

  List.rev !output |> List.to_array

let part1 () =
  let out = exec a in
  Array.iter out ~f:(printf "%d,")

let exec () =
  let a = ref 0 in
  let b = ref 0 in
  let c = ref 0 in

  let get_combo = function
    | x when x <= 3 -> x
    | 4 -> !a
    | 5 -> !b
    | 6 -> !c
    | _ -> failwith "invalid combo target"
  in

  let output = ref [] in

  let ip = ref 0 in

  let ins =
    List.chunks_of ~length:2 (List.of_array ins)
    |>  List.rev |> List.join |> List.to_array
  in

  let operand () = ins.(!ip + 1) in

  while !ip < Array.length ins do
    match ins.(!ip) with
    | 0 ->
        let op = operand () in
        a := !a * Int.pow 2 (get_combo op);
        ip := !ip + 2
    | 1 ->
        let op = operand () in

        b := Int.bit_xor !b op;
        ip := !ip + 2
    | 2 ->
        let op = operand () in
        b := get_combo op mod 8;
        ip := !ip + 2
    | 3 ->
        if !a <> 0 then
          let op = operand () in
          ip := op
        else ip := !ip + 2
    | 4 ->
        b := Int.bit_xor !b !c;
        ip := !ip + 2
    | 5 ->
        let op = operand () in
        let value = get_combo op mod 8 in
        output := value :: !output;
        ip := !ip + 2
    | 6 ->
        let op = operand () in
        b := !a / Int.pow 2 (get_combo op);
        ip := !ip + 2
    | 7 ->
        let op = operand () in
        c := !a / Int.pow 2 (get_combo op);
        ip := !ip + 2
    | _ -> failwith "invalid opcode"
  done

let part2 () = exec ()

let () =
  part1 ();
  part2 ()
