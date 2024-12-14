open Core

let width = 101
let height = 103

type pos = int * int
type robot = { pos : pos; vel : pos }

(* let ( +^ ) (ax, ay) (bx, by) = (ax + bx, ay + by) *)

let robots =
  Utils.read_lines "input/day14.txt"
  |> List.map ~f:(fun line ->
         let x, y, vx, vy =
           Scanf.sscanf line "p=%d,%d v=%d,%d" (fun x y vx vy -> (x, y, vx, vy))
         in
         { pos = (x, y); vel = (vx, vy) })

let part1 () =
  let robots = ref robots in
  for _ = 1 to 100 do
    robots :=
      List.map !robots ~f:(fun r ->
          let px, py = r.pos in
          let vx, vy = r.vel in

          let new_pos = ((px + vx) % width, (py + vy) % height) in
          { pos = new_pos; vel = r.vel })
  done;

  let q1 =
    List.count !robots ~f:(fun r ->
        let x, y = r.pos in
        x < width / 2 && y < height / 2)
  in

  let q2 =
    List.count !robots ~f:(fun r ->
        let x, y = r.pos in
        x < width / 2 && y > height / 2)
  in

  let q3 =
    List.count !robots ~f:(fun r ->
        let x, y = r.pos in
        x > width / 2 && y < height / 2)
  in

  let q4 =
    List.count !robots ~f:(fun r ->
        let x, y = r.pos in
        x > width / 2 && y > height / 2)
  in

  Utils.print_int (q1 * q2 * q3 * q4)

let part2 () =
  let robots =
    ref
      (Utils.read_lines "input/day14.txt"
      |> List.map ~f:(fun line ->
             let x, y, vx, vy =
               Scanf.sscanf line "p=%d,%d v=%d,%d" (fun x y vx vy ->
                   (x, y, vx, vy))
             in
             { pos = (x, y); vel = (vx, vy) }))
  in
  for t = 0 to 10000 do
    (* Create 10 000 ppm images, which can be converted to png with imagemagick: "convert *.ppm "*.png"" *)
    let out_file = Out_channel.create ("temp/" ^ string_of_int t ^ ".ppm") in

    Out_channel.output_line out_file "P3";
    Out_channel.output_line out_file
      (string_of_int width ^ " " ^ string_of_int height);
    Out_channel.output_line out_file "255";
    let grid = Array.(init height ~f:(fun _ -> create ~len:width 0)) in
    robots :=
      List.map !robots ~f:(fun r ->
          let px, py = r.pos in
          let vx, vy = r.vel in

          let new_x, new_y = ((px + vx) % width, (py + vy) % height) in
          grid.(new_y).(new_x) <- 1;
          { pos = (new_x, new_y); vel = r.vel });

    Array.iter grid ~f:(fun row ->
        Array.iter row ~f:(fun i ->
            if i = 0 then Out_channel.output_line out_file "0 0 0"
            else Out_channel.output_line out_file "255 255 255");
        ());

    printf "%d\n" t;
    Out_channel.flush out_file;
    ()
  done;

  ()

let () =
  part1 ();
  part2 ()
