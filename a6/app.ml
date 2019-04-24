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

let rec read_command (st: State.t) = 
  let command = read_line () in
  begin
    match parse command with
    | exception Empty -> st
    | exception Malformed -> print_endline "That's not a valid command!"; st
    | Disease -> let st' = {st with disease=print_disease_menu st.disease} in
      print_endline "Enter another command; type \"continue\" to continue.";
      read_command st';
    | Continue -> st;
    | Quit -> exit 0;
  end

(** [run_game st] is the main game loop that steps the game state [st],
    spreads disease within and between tiles, and prints out the map. *)
let rec run_game (st: State.t) =
  Unix.sleepf 0.1;
  Unix.set_nonblock Unix.stdin;
  let terminalio = Unix.tcgetattr Unix.stdin in
  begin
    match input_char Pervasives.stdin with
    | char -> 
      Unix.clear_nonblock Unix.stdin;
      Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { terminalio with 
                                                 Unix.c_icanon = true; 
                                                 Unix.c_echo = true};
      print_endline "\027[0m";
      let st' = read_command st in
      Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { terminalio with 
                                                 Unix.c_icanon = false; 
                                                 Unix.c_echo = false};
      run_game {st' with elapsed_time = st.elapsed_time + 1}
    | exception _ -> 
      print_string "\x1Bc";
      Unix.clear_nonblock Unix.stdin;
      TerminalPrint.print_map st.tiles st.elapsed_time;
      TerminalPrint.print_living_dead st;
      TerminalPrint.print_infected st;
      TerminalPrint.print_population st;
      TerminalPrint.print_world_info st;
      flush Pervasives.stdout;

      if (total_dead st) = (total_population st) then
        (print_endline "\027[0m\nCongratulations! You won!";
         Unix.tcsetattr Unix.stdin Unix.TCSADRAIN {terminalio with 
                                                   Unix.c_icanon = true; 
                                                   Unix.c_echo = true};
         exit 0)
      else
        let disease = st.disease in
        let tiles = st.tiles in

        (for x = 0 to Array.length tiles - 1 do
           for y = 0 to Array.length tiles.(x) - 1 do
             check_neighbors tiles x y disease;
             tiles.(x).(y) <- infect_tile (tiles.(x).(y))(disease);
           done
         done);
        run_game {st with elapsed_time = st.elapsed_time + 1}
  end

(** [start_game state start_coordinates] starts the game with a given state
    [state] (with the map, disease, and civilizations) and starts the disease at 
    a given location [start_coordinates] inputted by the user.*)
let rec start_game (state: State.t) (start_coordinates : string) =
  Random.self_init ();
  let terminalio = Unix.tcgetattr Unix.stdin in
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
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN {terminalio with 
                                                  Unix.c_icanon = false; 
                                                  Unix.c_echo = false};
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
let setup_game =
  ANSITerminal.resize 200 45;
  print_string "\x1Bc";
  print_string 
    "\n\n\n\n\n\n\n\n\n\n\x1B[38;2;179;137;179m
                                                                        ▀█████████▄   ▄█        ▄█     ▄██████▄     ▄█    █▄        ███    \x1B[38;2;145;103;145m 
                                                                          ███    ███ ███       ███    ███    ███   ███    ███   ▀█████████▄ \x1B[38;2;117;73;117m
                                                                          ███    ███ ███       ███▌   ███    █▀    ███    ███      ▀███▀▀██ \x1B[38;2;94;47;94m
                                                                         ▄███▄▄▄██▀  ███       ███▌  ▄███         ▄███▄▄▄▄███▄▄     ███   ▀ \x1B[38;2;81;32;81m
                                                                        ▀▀███▀▀▀██▄  ███       ███▌ ▀▀███ ████▄  ▀▀███▀▀▀▀███▀      ███    \x1B[38;2;73;26;73m
                                                                          ███    ██▄ ███       ███    ███    ███   ███    ███       ███     \x1B[38;2;70;14;70m
                                                                          ███    ███ ███▌    ▄ ███    ███    ███   ███    ███       ███     \x1B[38;2;60;6;60m
                                                                        ▄█████████▀  █████▄▄██ █▀     ████████▀    ███    █▀       ▄████▀  \x1B[38;2;51;0;51m
    \027[0m\n";

  print_string
    "\n\n\n
                                                  _____  _        _                     _                _    _                     _  _                                 
                                                 |  __ \(_)      | |                   | |              | |  (_)                   | |(_)                                _ 
                                                 | |__) |_   ___ | | __    __ _    ___ | |_  __ _  _ __ | |_  _  _ __    __ _    __| | _  ___   ___   __ _  ___   ___   (_)
                                                 |  ___/| | / __|| |/ /   / _` |  / __|| __|/ _` || '__|| __|| || '_ \\  / _` |  / _` || |/ __| / _ \\ / _` |/ __| / _ \\   
                                                 | |    | || (__ |   <   | (_| |  \__ \| |_| (_| || |   | |_ | || | | || (_| | | (_| || |\__ \|  __/| (_| |\__ \|  __/   _ 
                                                 |_|    |_| \___||_|\\_\\   \\__,_|  |___/ \\__|\\__,_||_|    \\__||_||_| |_| \\__, |  \\__,_||_||___/ \\___| \\__,_||___/ \\___|  (_)
                                                                                                                         __/ |                                           
                                                                                                                         |___/                                            ";

  print_string
    "\n\n\n
                                                    __________________              __________________              __________________              __________________        
                                                   /                  \\            /                  \\            /                  \\            /                  \\       
                                                  /        ,-^-.       \\          /                    \\          /                    \\          /                    \\      
                                                 /         |\\/\\|        \\        /   (,_    ,_,    _,)  \\        /      |///////|       \\        /                      \\     
                                                /          `-V-'         \\      /    /|\\`-._( )_.-'/|\\   \\      /       | _   _ |        \\      /                        \\    
                                               /             H            \\    /    / | \\`-'/ \\'-`/ | \\   \\    /       ( (o) (o) )        \\    /                          \\   
                                               \\             H            /    \\   /__|.-'`-\\_/-`'-.|__\\  /    \\        |  . .  |         /    \\                          /   
                                                \\         .-;\":-.        /      \\            v           /      \\        \\  _  /         /      \\                        /    
                                                 \\      ,'|  v  |',     /        \\                      /        \\        \\___/         /        \\                      /     
                                                  \\                    /          \\                    /          \\                    /          \\                    /      
                                                   \\__________________/            \\__________________/            \\__________________/            \\__________________/       

                                                          Ebola                            Rabies                         Cooties                         Custom
                                                           (a)                              (b)                             (c)                             (d)
  \n";

  let selection = read_line () in
  let temp_state = 
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
    let disease = Objects.cooties_default in
    let civcoord = Array.make 5 (0,0) in
    State.{civilizations = [civ1]; 
           disease = disease; 
           tiles = map; 
           elapsed_time = 0;
           civcoords = civcoord} in
  if selection = "a" || selection = "ebola" then
    let state = {temp_state with disease = Objects.ebola_default} in
    start_game state "10 10"
  else if selection = "b" || selection = "rabies" then
    let state = {temp_state with disease = Objects.rabies_default} in
    start_game state "10 10"
  else if selection = "c" || selection = "cooties" then
    start_game temp_state "10 10"

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
         (Tile.{tile_type = Land;
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
  setup_game

let () = main ()