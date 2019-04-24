open TerminalPrint
open Controller
open Objects
open Infection
open State
open RandomMap

(** [string_to_list str] splits a string [str] at every space into a list. *)
let string_to_list str =
  let command_list = (String.split_on_char ' ' str) in 
  List.filter (fun ele -> ele <> "") command_list

(** [run_game st] is the main game loop that steps the game state [st],
    spreads disease within and between tiles, and prints out the map. *)
let rec run_game (st: State.t) =
  (* let civilizations = st.civilizations in *)
  let user_input command = 
    match Controller.parse command with
    | exception Empty -> print_endline "You need to input something!";
      (match input_char Pervasives.stdin with
       | char -> run_game {st with elapsed_time = st.elapsed_time + 1}
       | exception _ -> ())
    | exception Malformed -> print_endline "Sorry, something went wrong with your input.";
      (match input_char Pervasives.stdin with
       | char -> run_game {st with elapsed_time = st.elapsed_time + 1}
       | exception _ -> ())
    | Quit -> print_endline "Thanks for playing!"; exit 0
    | Disease -> Controller.print_disease_menu st.disease; 
      (let disease_command = read_line () in 
       match Controller.disease_parse disease_command st.disease with 
       | exception Disease_Malformed -> print_endline "Sorry, your input was not formatted correctly.";
         (match input_char Pervasives.stdin with
          | char -> run_game {st with elapsed_time = st.elapsed_time + 1}
          | exception _ -> ())
       | disease -> run_game {st with disease = disease}) in
  print_string "\x1Bc";
  Unix.set_nonblock Unix.stdin;
  begin
    match input_char Pervasives.stdin with
    | char -> 
      Unix.clear_nonblock Unix.stdin;
      print_endline "Help:\nType [disease] to view the current disease stats and change them if needed. Type [quit] to quit the game."; 
      let command = read_line () in user_input command
    | exception _ -> 
      print_string "\x1Bc";
      Unix.clear_nonblock Unix.stdin;
      TerminalPrint.print_map st.tiles st.elapsed_time;
      TerminalPrint.print_living_dead st;
      TerminalPrint.print_infected st;
      TerminalPrint.print_population st;
      print_endline "If you would like to input a command, press [Enter] and wait.\nDo not input anything else!";
      flush Pervasives.stdout;
      let disease = st.disease in
      let tiles = st.tiles in

      (for x = 0 to Array.length tiles - 1 do
         for y = 0 to Array.length tiles.(x) - 1 do
           check_neighbors tiles x y disease;
           tiles.(x).(y) <- infect_tile (tiles.(x).(y))(disease);
         done
       done);
  end;
  Unix.sleepf 1.0;
  run_game {st with elapsed_time = st.elapsed_time + 1}

(** [start_game state start_coordinates] starts the game with a given state
    [state] (with the map, disease, and civilizations) and starts the disease at 
    a given location [start_coordinates] inputted by the user.*)
let rec start_game (state: State.t) (start_coordinates : string) =
  Random.self_init ();
  let terminalio = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN {terminalio with 
                                            Unix.c_icanon = false; 
                                            Unix.c_echo = false};
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
      if (Array.length state.tiles > x) && (Array.length state.tiles.(0) > y)
         && (x > 0) && (y > 0)
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

(** [condition_check] checks is given in the range from 0 to 100.  If not, 
    the user is prompoted to try again  *)
let rec condition_check prob = 
  try let spread = int_of_string (prob) in
    if (spread >= 0) && (spread <= 100)
    then spread
    else 
      (print_endline "\027[31mMake sure the integer is between 0-100.\027[0m";
       print_string "> ";
       match read_line () with
       | input -> condition_check input)

  with Failure int_of_string ->
    print_endline "\027[31mMake sure you enter an integer!\027[0m";
    print_string "> ";
    match read_line () with
    | input -> condition_check input

(** [coordinate_check] checks if coordinates are valid from a pair of numbers
    bigger than 0.  If not, the user is prompoted again  *)
let rec coordinate_check prob = 
  try let xy = List.map int_of_string (string_to_list prob) in
    if List.length xy <> 2 
    then (print_endline "\027[31mYou need exactly two numbers!\027[0m";
          print_string "> ";
          match read_line () with
          | input -> coordinate_check input)
    else
      let x = List.hd xy in
      let y = List.nth xy 1 in
      if (x > 0) && (y > 0)
      then xy
      else
        (print_endline "\027[31mCoordinates are out of bounds!\027[0m";
         print_string "> ";
         match read_line () with
         | input -> coordinate_check input)
  with Failure string -> 
    print_endline "\027[31mMake sure you enter two integers!\027[0m";
    print_string "> ";
    match read_line () with
    | input -> coordinate_check input


(** [setup_disease] lets the user initialize the map with a civilization
    and choose different values for the disease they want to place into the
    world. *)
let rec setup_disease =
  print_string "\x1Bc";
  print_string 
    "\n\n\n\027[31m    
    ▀█████████▄   ▄█        ▄█     ▄██████▄     ▄█    █▄        ███     
      ███    ███ ███       ███    ███    ███   ███    ███   ▀█████████▄ 
      ███    ███ ███       ███▌   ███    █▀    ███    ███      ▀███▀▀██ 
     ▄███▄▄▄██▀  ███       ███▌  ▄███         ▄███▄▄▄▄███▄▄     ███   ▀ 
    ▀▀███▀▀▀██▄  ███       ███▌ ▀▀███ ████▄  ▀▀███▀▀▀▀███▀      ███    
      ███    ██▄ ███       ███    ███    ███   ███    ███       ███     
      ███    ███ ███▌    ▄ ███    ███    ███   ███    ███       ███     
    ▄█████████▀  █████▄▄██ █▀     ████████▀    ███    █▀       ▄████▀  
    \027[0m\n\n\n";

  print_endline "Would you like to use default or custom parameters?";
  print_string "> ";
  let command = read_line () in
  if command = "default" then
    let state = 
      let civ1 = 
        Civilization.{infected = ref 0; 
                      living = ref 40000;
                      dead = ref 0;
                      population = 40000; 
                      neighbors= []} in
      let map = Array.make_matrix 20 20
          Tile.{tile_type = (Civ civ1); 
                infected = 0; 
                living = 100;
                dead = 0;
                population = 100} in
      let disease = Disease.{inner_tile_spread = 20; 
                             tile_to_tile_spread = 80;
                             water_spread = 50;
                             road_spread = 30;
                             civ_to_civ_spread = 0;
                             spread_probability = 10;
                             lethality = 10} in
      let civcoord = Array.make 5 (0,0) in
      State.{civilizations = [civ1]; 
             disease = disease; 
             tiles = map; 
             elapsed_time = 0;
             civcoords = civcoord} in
    start_game state "10 10"

  else
    (print_string "\x1Bc";
     print_endline "Enter your map size in the form \"height length\": ";
     print_string "> ";
     let map_size = read_line () in
     let xy = coordinate_check map_size in

     print_endline "Enter inner tile spread: ";
     print_string "> ";
     let prob = read_line () in 
     let inner_tile_spread = condition_check prob in 

     print_endline "Enter tile to tile spread: ";
     print_string "> ";
     let prob = read_line () in
     let tile_to_tile_spread = condition_check prob in

     print_endline "Enter spread probability: ";
     print_string "> ";
     let prob = read_line () in
     let spread_probability = condition_check prob in

     print_endline "Enter the disease lethality: ";
     print_string "> ";
     let prob = read_line () in  
     let lethality = condition_check prob in

     print_endline "Enter the starting coordinates in the form \"x y\"";
     print_string "> ";
     let starting_coordinates = read_line () in

     let map = Array.make_matrix (List.hd xy) (List.nth xy 1) 
         (Tile.{tile_type = Road 0;
                infected = 0;
                living =0;
                dead = 0;
                population= 0}) in
     let civcoord = Array.make 5 (0,0) in
     let state = 
       let civ1 = 
         Civilization.{infected = ref 0; 
                       living = ref (100 * (List.hd xy) * (List.nth xy 1));
                       dead = ref 0;
                       population = 100 * (List.hd xy) * (List.nth xy 1); 
                       neighbors= []} in

       let disease = Disease.{inner_tile_spread = inner_tile_spread; 
                              tile_to_tile_spread = tile_to_tile_spread;
                              water_spread = 50;
                              road_spread = 30;
                              civ_to_civ_spread = 0;
                              spread_probability = spread_probability;
                              lethality = lethality} in
       State.{civilizations = [civ1]; 
              disease = disease; 
              tiles = map; 
              elapsed_time = 0;
              civcoords = civcoord} in
     generateMap state.tiles state.civcoords 10;
     start_game state starting_coordinates)

(** [main ()] starts the game and prompts the user for the starting coordinates
    of the disease. *)
let main () = 
  setup_disease

let () = main ()