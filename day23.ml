open Core

let input = Utils.read_lines "input/day23.txt"
let connections = Hashtbl.create (module String)
let neighbors v = Hashtbl.find_multi connections v

let () =
  List.iter input ~f:(fun line ->
      let a, b = Scanf.sscanf line "%[a-z]-%[a-z]" Tuple2.create in
      Hashtbl.add_multi connections ~key:a ~data:b;
      Hashtbl.add_multi connections ~key:b ~data:a)

let vertexes = Hashtbl.keys connections |> Hash_set.of_list (module String)

let sets_of_three connections =
  let sets = Utils.TripleHashSet.create () in

  let connected_to a b = List.mem ~equal:equal_string (neighbors a) b in
  Hashtbl.iteri connections ~f:(fun ~key ~data ->
      (* for every computer connected to ~key *)
      List.iter data ~f:(fun comp2 ->
          (* Iterate those connected to comp2 *)
          List.iter (Hashtbl.find_multi connections comp2) ~f:(fun third ->
              if connected_to key third then
                let sorted =
                  List.sort [ key; comp2; third ] ~compare:String.compare
                in
                match sorted with
                | [ a; b; c ] -> Hash_set.add sets (a, b, c)
                | _ -> failwith "???")));

  sets

let part1 () =
  let s =
    sets_of_three connections
    |> Hash_set.filter ~f:(fun t ->
           let a, b, c = Tuple3.map t ~f:(String.is_prefix ~prefix:"t") in
           a || b || c)
  in
  s |> Hash_set.length |> Utils.print_int

let hash_set_of = Hash_set.of_list (module String)

let rec bronKerbosch r p x =
  let open Hash_set in
  if is_empty p && is_empty x then r
  else
    let new_p = Hash_set.copy p in
    let max = ref @@ hash_set_of [] in
    Hash_set.iter p ~f:(fun v ->
        let temp =
          bronKerbosch
            (union r (hash_set_of [ v ]))
            (inter new_p (hash_set_of (neighbors v)))
            (inter x (hash_set_of (neighbors v)))
        in
        if length temp > length !max then max := temp;

        Hash_set.remove new_p v;
        Hash_set.add x v);
    !max

let part2 () =
  bronKerbosch (hash_set_of []) vertexes (hash_set_of [])
  |> Hash_set.to_list
  |> List.sort ~compare:compare_string
  |> List.fold ~init:"" ~f:(fun acc str -> acc ^ "," ^ str)
  |> printf "%s"

let () =
  part1 ();
  part2 ()
