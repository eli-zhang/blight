open Objects 
open State

(** [print_map map time] prints the map [map] followed by the time [time]
    elapsed so far in the world. *)
let print_map map time = 
  let rec printMap_helper (tile: Tile.t) =
    let colors = true in
    if colors then 
      (* Prints the map in colored tiles *)
      match tile.tile_type with
      | Land -> print_string "\027[40m  "; 
      | Water -> print_string "\027[46m  "; 
      | Road -> print_string "\027[0m  ";
      | Civ civ -> 
        let infected = tile.infected in 
        let population = tile.population in
        let ratio = 100 * infected / population in
        if ratio > 0 then 
          if ratio = 100 then (print_string "\027[41m  ") else
          if ratio > 66 then (print_string "\027[31m\027[47m::")
          else (print_string "\027[31m\027[47m··")
        else (print_string "\027[47m  ";)

    else 
      (* Prints information about each tile in the map *)
      match tile.tile_type with
      | Land -> print_string "\027[40m L"; 
      | Water -> print_string "\027[46m W"; 
      | Road -> print_string "\027[0m R";
      | Civ civ -> 
        let infected = tile.infected in 
        let population = tile.population in
        print_string ("I: " ^ string_of_int infected 
                      ^ " P: " ^ string_of_int population ^ " "); in

  let rec printMap_helper2 map count time =
    if count = (Array.length map) then 
      (print_endline ("\027[31mElapsed Time: " ^ (string_of_int time)))
    else (Array.iter printMap_helper (Array.get map count); 
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

(** [print_infected state] prints the total number of infected people in
    the world given by state [state]. *)
let print_infected (state: State.t) =
  let total_infected = 
    List.fold_left (fun acc (civ: Civilization.t) -> 
        acc + !(civ.infected)) 0 state.civilizations in
  print_endline("\027[31mTotal infected: " 
                ^ string_of_int total_infected)

(** [print_population state] prints the total number of people in the world
    given by state [state]. *)
let print_population (state: State.t) =
  let total_population =
    List.fold_left (fun acc (civ: Civilization.t) -> acc + civ.population) 
      0 state.civilizations in
  print_endline("\027[31mTotal population: "
                ^ string_of_int total_population)
