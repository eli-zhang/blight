type tile = | Land | Water | Air
let world = [(1, 1, Land); (1, 2, Land); (1, 3, Land); (1, 4, Land);
             (2, 1, Land); (2, 2, Land); (2, 3, Land); (2, 4, Land);
             (3, 1, Land); (3, 2, Land); (3, 3, Water); (3, 4, Water);
             (4, 1, Water); (4, 2, Water); (4, 3, Water); (4, 4, Water)]

let print_world world =
  let rec print_world_helper row world = 
    match world with
    | [] -> ANSITerminal.(print_string [red] "\nDone.");
    | (a, b, tile)::t ->
      if a = row then 
        begin
          match tile with
          | Land -> ANSITerminal.(print_string [black] "▉"); 
            print_world_helper a t 
          | Water -> ANSITerminal.(print_string [blue] "▉"); 
            print_world_helper a t 
          | Air -> ANSITerminal.(print_string [white] "▉"); 
            print_world_helper a t 
        end
      else
        begin
          match tile with
          | Land -> ANSITerminal.(print_string [black] "\n▉"); 
            print_world_helper a t
          | Water -> ANSITerminal.(print_string [blue] "\n▉"); 
            print_world_helper a t 
          | Air -> ANSITerminal.(print_string [white] "\n▉"); 
            print_world_helper a t 
        end
    | _ -> failwith "Incorrect world format" in
  print_world_helper 1 world


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_world world

(* Execute the game engine. *)
let () = main ()
