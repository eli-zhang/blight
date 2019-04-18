open TerminalPrint
(* open RandomMap *)
open Controller
open Unix
open Objects
open Infection
open State

(** [string_to_list str] splits a string [str] at every space into a list. *)
let string_to_list str =
  let command_list = (String.split_on_char ' ' str) in 
  List.filter (fun ele -> ele <> "") command_list

(** [run_game st] is the main game loop that steps the game state [st],
    spreads disease within and between tiles, and prints out the map. *)
let rec run_game (st: State.t) =
  (* let civilizations = st.civilizations in *)
  print_string "\x1Bc";
  TerminalPrint.print_map st.tiles st.elapsed_time;
  TerminalPrint.print_infected st;
  TerminalPrint.print_population st;
  flush Pervasives.stdout;
  let disease = st.disease in
  let tiles = st.tiles in

  (for x = 0 to Array.length tiles - 1 do
     for y = 0 to Array.length tiles.(x) - 1 do
       check_neighbors tiles x y disease;
       tiles.(x).(y) <- infect_tile (tiles.(x).(y))(disease);
     done
   done);
  (* let updated_civilizations = 
     infect_civilizations [] civilizations disease in *)
  Unix.sleepf 0.1;
  run_game {st with elapsed_time = st.elapsed_time + 1}

(** [start_game state start_coordinates] starts the game with a given state
    [state] (with the map, disease, and civilizations) and starts the disease at 
    a given location [start_coordinates] inputted by the user.*)
let rec start_game (state: State.t) (start_coordinates : string) =
  let string_list = string_to_list start_coordinates in
  try let xy = List.map int_of_string string_list in
    if List.length xy <> 2 
    then (print_endline "\027[31mYou need exactly two numbers!\027[0m";
          print_string "> ";
          match read_line () with
          | exception End_of_file -> ()
          | input -> start_game state input)
    else
      let x = List.hd xy in
      let y = List.nth xy 1 in
      if (Array.length state.tiles > x) && (Array.length state.tiles.(x) > y)
      || (x > 0) || (y > 0)
      then 
        let state_with_coordinates = 
          state.tiles.(x).(y) 
          <- start_tile_infection state.tiles.(x).(y);
          state in
        run_game state_with_coordinates
      else
        (print_endline "\027[31mCoordinates are out of bounds!\027[0m";
         print_string "> ";
         match read_line () with
         | exception End_of_file -> ()
         | input -> start_game state input)

  with Failure string -> 
    print_endline "\027[31mMake sure you enter two integers!\027[0m";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | input -> start_game state input

(** [setup_disease] lets the user initialize the map with a civilization
    and choose different values for the disease they want to place into the
    world. *)
let rec setup_disease =
  print_string "\x1Bc";
  print_endline "Enter your map size in the form \"height length\": ";
  print_string "> ";
  let map_size = read_line () in
  let xy = List.map int_of_string (string_to_list map_size) in

  print_endline "Enter inner tile spread: ";
  print_string "> ";
  let inner_tile_spread = int_of_string (read_line ()) in

  print_endline "Enter tile to tile spread: ";
  print_string "> ";
  let tile_to_tile_spread = int_of_string (read_line ()) in

  print_endline "Enter spread probability: ";
  print_string "> ";
  let spread_probability = int_of_string (read_line ()) in

  print_endline "Enter the starting coordinates in the form \"x y\"";
  print_string "> ";
  let starting_coordinates = read_line () in

  let state = 
    let civ1 = 
      Civilization.{infected = ref 0; 
                    population = 100 * (List.hd xy) * (List.nth xy 1); 
                    neighbors= []} in
    let map = Array.make_matrix (List.hd xy) (List.nth xy 1)
        Tile.{tile_type = (Civ civ1); infected = 0; population = 100} in
    let disease = Disease.{inner_tile_spread = inner_tile_spread; 
                           tile_to_tile_spread = tile_to_tile_spread; 
                           civ_to_civ_spread = 0;
                           spread_probability = spread_probability} in
    State.{civilizations = [civ1]; 
           disease = disease; 
           tiles = map; 
           elapsed_time = 0} in
  start_game state starting_coordinates

(** [main ()] starts the game and prompts the user for the starting coordinates
    of the disease. *)
let main () = 
  setup_disease

let () = main ()