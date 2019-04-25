open Objects 
open State

(** [print_map map time] prints the map [map] followed by the time [time]
    elapsed so far in the world. *)
let print_map map time = 
  let rec printMap_helper counter (tile: Tile.t) =
    let colors = true in
    if colors then 
      (* Prints the map in colored tiles *)
      match tile.tile_type with
      | Land -> 
        counter := !(counter) + 1;
        if !(counter) mod 3 = 0 then print_string "\x1B[48;2;0;100;0m  "
        else if !(counter) mod 3 = 1 then print_string "\x1B[48;2;34;139;34m  "
        else print_string "\x1B[48;2;0;128;0m  ";
      | Water percentage -> 
        if percentage > 0 then
          if percentage = 100 then (print_string "\x1B[48;2;153;255;51m  ") else
          if percentage > 66 then (print_string "\x1B[48;2;153;255;153m  ") else
            (print_string "\x1B[48;2;102;255;178m  ") 
        else (print_string "\x1B[48;2;153;255;204m  ");
      | Road percentage -> 
        if percentage > 0 then
          if percentage = 100 then (print_string "\x1B[48;2;102;102;0m  ") else
          if percentage > 66 then (print_string "\x1B[48;2;112;61;0m\x1B[38;2;102;102;0m::") else
            (print_string "\x1B[48;2;112;61;0m\x1B[38;2;102;102;0m··") 
        else (print_string "\x1B[48;2;112;61;0m  ";);
      | Civ civ -> 
        let infected = tile.infected in
        let dead = tile.dead in
        let population = tile.population in
        let infected_ratio = 100 * infected / population in
        let dead_ratio = 100 * dead / population in
        if dead_ratio = 0 && infected_ratio = 0 then (print_string "\027[47m  ")
        else if dead_ratio = 0 && infected_ratio = 100 then (print_string "\027[41m  ")
        else if dead_ratio = 0 && infected_ratio > 66 then (print_string "\027[31m\027[47m::")
        else if dead_ratio = 0 then (print_string "\027[31m\027[47m··")
        else if dead_ratio = 100 then (print_string "\x1B[48;2;0;0;0m  ")
        else if dead_ratio > 66 && infected_ratio = 100 then (print_string "\027[41m\x1B[38;2;0;0;0m::")
        else if dead_ratio > 0 && infected_ratio = 100 then (print_string "\027[41m\x1B[38;2;0;0;0m··")
        else if dead_ratio > 33 && infected_ratio > 66 then (print_string "\027[47m\x1B[38;2;0;0;0m:\027[31m:")
        else if dead_ratio > 0 && infected_ratio > 66 then (print_string "\027[41m::")
        else if dead_ratio > 0 && infected_ratio > 0 then (print_string "\027[47m\x1B[38;2;0;0;0m·\027[31m·")
        else (print_string "\027[47m  ";)

    else 
      (* Prints information about each tile in the map *)
      match tile.tile_type with
      | Land -> print_string "\027[40m L"; 
      | Water _ -> print_string "\027[46m W"; 
      | Road _ -> print_string "\027[0m R";
      | Civ civ -> 
        let infected = tile.infected in 
        let population = tile.population in
        print_string ("I: " ^ string_of_int infected 
                      ^ " P: " ^ string_of_int population ^ " "); in

  let rec printMap_helper2 map count time =
    let tile_color_counter = ref 0 in
    if count = (Array.length map) then 
      (print_endline ("\027[31mElapsed Time: " ^ (string_of_int time)))
    else (Array.iter (printMap_helper tile_color_counter) (Array.get map count); 
          print_string "\027[0m\n"; printMap_helper2 map (count+1) time)
  in (print_string "\027[0;0H"; printMap_helper2  map 0 time)

let print_living_dead (state: State.t) =
  let living_dead_count = 
    List.fold_left (fun acc (civ: Civilization.t) -> 
        (fst acc + !(civ.living), snd acc + !(civ.dead))) 
      (0,0) state.civilizations in
  print_endline("\027[31mTotal living: " 
                ^ string_of_int (fst living_dead_count));
  print_endline("\027[31mTotal dead: "
                ^ string_of_int (snd living_dead_count))

let total_dead (state: State.t) =
  List.fold_left (fun acc (civ: Civilization.t) -> 
      acc + !(civ.dead)) 0 state.civilizations

let total_infected (state: State.t) = 
  List.fold_left (fun acc (civ: Civilization.t) -> 
      acc + !(civ.infected)) 0 state.civilizations

(** [print_infected state] prints the total number of infected people in
    the world given by state [state]. *)
let print_infected (state: State.t) =
  print_endline("\027[31mTotal infected: " 
                ^ string_of_int (total_infected state))

let total_population (state: State.t) =
  List.fold_left (fun acc (civ: Civilization.t) -> acc + civ.population) 
    0 state.civilizations

(** [print_population state] prints the total number of people in the world
    given by state [state]. *)
let print_population (state: State.t) =
  print_endline("\027[31mTotal population: "
                ^ string_of_int (total_population state))

let print_world_info (state: State.t) =
  let change_tup (a, b, c, d) a' b' c' d' = (a' + a, b' + b, c' + c, d' + d) in
  let stats =
    List.fold_left (fun acc (civ: Civilization.t) -> 
        change_tup acc !(civ.living) !(civ.dead) !(civ.infected) civ.population)
      (0, 0, 0, 0) state.civilizations in
  match stats with
  | (living, dead, infected, population) ->
    let living_percent = 100.0 *. (float_of_int living) /. (float_of_int population) in
    let dead_percent = 100.0 *. (float_of_int dead) /. (float_of_int population) in
    let infected_percent =  100.0 *. (float_of_int infected) /. (float_of_int population) in
    let rec print_bar_helper percent color =
      let threshold = 1.0 in
      if percent >= threshold 
      then (print_string (color ^ " ");
            print_bar_helper (percent -. threshold) color); in
    print_bar_helper (dead_percent) "\x1B[48;2;10;10;10m";
    print_bar_helper (infected_percent -. dead_percent) "\x1B[48;2;178;34;34m";
    print_bar_helper (100.0 -. infected_percent) "\x1B[48;2;160;160;160m";
    print_endline "\027[0m\n"
