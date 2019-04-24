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

let setup_disease state coords =
  print_string "\x1Bc
\027[0m                                              \x1B[38;2;51;0;0m                                                                          :%SSttXX88t 888 8888888@%X t            X@S 8          XS8S8X     ;S8SX888@:    
\027[0m                                              \x1B[38;2;70;0;0m                                                                      :8%@S8S88888XS@ 8t8%;;tt ;8 8X88;          X8@;88;:        8X t@8:   t8Xt8888X      
\027[0m                                              \x1B[38;2;91;0;0m                                                                     %XS8S8 8 :888S8 tS:         8888@S          S 8@X8:         @8;%XX   @@8X 8X88:      
\027[0m                                              \x1B[38;2;111;0;0m                                                                   :%8@8X888X;88S88 :            S88888          ;8@8SXX:        8XS88   @X88 888 ;       
\027[0m                                              \x1B[38;2;133;0;0m                                                                  XtX@88888%88SX; 88t@           :%8@8SS          %88X88;        8;8tX  S8%88 8:@         
\027[0m                                              \x1B[38;2;144;0;0m                                                                ;XXXXX888;8888t tt8tX :            8S8@8:         :@8S88X        SX8 8:8@8X88888          
\027[0m                                              \x1B[38;2;155;0;0m                                                              :@S@X888X88 X@t:   ;8X@8 ;           8;@88:          8;t8X8       ;@8 88@@888@ 88t          
\027[0m                                              \x1B[38;2;166;0;0m                                                             %@88@@S SX8;t        @%8S@%%           @ X88@        : X8S:8:       S 8 88X888;8X:           
\027[0m                                              \x1B[38;2;173;0;0m                                                           S888X8;S@XS8S;          8@8888           ;8%88 :         8888%       : :X8X8S@888;             
\027[0m                                              \x1B[38;2;180;0;0m                                                         :tS%@8X8@%8888t           X @88@8t         ;@S88X8         t;;SX@       %88S888 @ S              
\027[0m                                              \x1B[38;2;200;0;0m                                                       :@S888XX8 S88888S@           ;88;8X%;         :8S 8St         88888:      8S:@ ;@:88               
\027[0m                                              \x1B[38;2;211;0;0m                                                       t888X888St88%88X@ :           ;tX8@@           % S88S%        X  8        8@S8S888X;               
\027[0m                                              \x1B[38;2;223;0;0m                                                     S8888 88@8 S: %888%S@             8X @tt          ;8888 ;        88S;8:   :%@tt@8@X 8                
\027[0m                                              \x1B[38;2;211;0;0m                                                    t8@888;@S ;;    :S 888@X           %88%8X8         :8XX%88        88@@XS  t8888  ::8t                 
\027[0m                                              \x1B[38;2;200;0;0m                                                  :X888@8@X8X8;       ;%@8;@:%          S8t 8@;         X 8X8         ; 88SS  %88888X88t                  
\027[0m                                              \x1B[38;2;180;0;0m                                                 ;8@;8888X8 :t         X8@;88;           X%%88@          tX8:X%        8@S88 8S:8% 8  8                   
\027[0m                                              \x1B[38;2;170;0;0m                                                S@S@88%8t 8X            ;;t@8XX8          X@88X%S        ::@88X%      ;888%888 8S8%%S                     
\027[0m                                              \x1B[38;2;160;0;0m                                              :SX@88%@S@S@ S              %:; 8;          :888SXSt        X@8S88;     :8S88%@@888%8;                      
\027[0m                                              \x1B[38;2;155;0;0m                                             ;XSS888@88  8S:S8@            888X8SX         :St8XS8        ;X;X88X      @ 88S8@X888%                       
\027[0m                                              \x1B[38;2;144;0;0m    ::;;;:%%S%;:  :tt:                       8@8888S888@X888 8 ;:          :S8888;8X:        8X888Xt       : 8888@    SStXS8%t@ 88                        
\027[0m                                              \x1B[38;2;120;0;0mS;X@XS88XX8888X%88%8@8XX@t;SSt;:             St@888 8S;  :8X 8888t           :SX8@88@        ;88X8t8:       X 888X;  t88 8S8X  8X                         
\027[0m                                              \x1B[38;2;111;0;0mS@X@S888S8SS888%S@88@X8@X888@8@;@8888SSt;:;8S88X@S%8;     S88:8X 88;          :8  :8;        :@%XS8@8;       % 8@ 888S8 @8S8X 8%                          
\027[0m                                              \x1B[38;2;104;0;0m8@X888XXSX8888XX8@%88%888888@@%8@%8X88888@@8 :SXS X X        @888:88 8@        ;%888S 8         @ X88SS       8  8Xt88%@8 8 88;                           
\027[0m                                              \x1B[38;2;154;0;0m@S@8XX@8888888888888888@888%88888tS8888888S888888;8tSt8S@8X8% ;S8SX8X8S8         @: @S 8:        8888%8:      @S@t; 88tX:@%@;                             
\027[0m                                              \x1B[38;2;180;0;0mX888888@888;SX@8888888@t;XX88@8888888@888888X8S888@888888 X8 8XX888tt8X@S;:       :SS%8@ ;        88:@8S @@@XX8X88t@X%S X 8t                              
\027[0m                                              \x1B[38;2;220;0;0m8888 88888S8%8888%@8888@8@888:88888%  SS::XSSSX8@@@888@8SS%@@;t88@ 8S   88S;@;@%8S8SXX@@@Xt::   : 8%88 XXXX;888;8@ 88:888;:                               
\027[0m                                              \x1B[38;2;255;0;0m88@8Xtt;;       ;S88888X :;;::%t;tX88888StX8%S@@X8SX8888@88888X8S888888 88S8888S@@88%8t88%8888@S8@XS %S 88 8t%tX888t888;:                                 
\027[0m                                              \x1B[38;2;215;0;0m88888            8XS8@ :          88888S8S8@ 8888888 t:%t8XX88@XX88888X8888X@8@8X XX8S%8 StX8888X8X@8StX88% 8SSS@X@8S@t                                   
\027[0m                                              \x1B[38;2;180;0;0m SXS88           t 88@ :         SX%888X8X8 X::;;;;t    %88@88888 8tX@XX@8@888S88S @8@S88X 8XX::8 SX888 @X @8t88%8;                                       
\027[0m                                              \x1B[38;2;150;0;0m88888:;          ;X@X@X8        :888888@88@:                   :;:tS8%88%%S8X % @8%t@8 @8tS8 S8;88:888 X8 X @%@:t:                                        
\027[0m                                              \x1B[38;2;125;0;0m8888888          tt@88@8:       t88X8 XSXtS                         ::t;   ;St;X:t88S%8;8S: 8XXS 8t@X @8X88t:                                             
\027[0m                                              \x1B[38;2;100;0;0m;;X8@%:            8@8 8       t@8888;:@8:                                            ;    ;SX::::%St                                                     
\027[0m                                              \x1B[38;2;105;0;0m %X@88@t          S8S8@ :     %X8%8888 S                                                      :                                                           
\027[0m                                              \x1B[38;2;110;0;0m ;S888X%          St88888   :tSt8% :88%X                                                                                                                  
\027[0m                                              \x1B[38;2;125;0;0m  tt888@X         X @8XXX   S888tS8@ @t                                                                                                                   
\027[0m                                              \x1B[38;2;135;0;0m   t88888         %;X %88% tS 88X888S :                                                                                                                   
\027[0m                                              \x1B[38;2;143;0;0m   ;t8@88X         ;t@88 S8S8@888XSt;                                                                                                                     
\027[0m                                              \x1B[38;2;158;0;0m     @8@8 t        :XS@8%%X8t@88888:                                                                                                                      
\027[0m                                              \x1B[38;2;165;0;0m     @S88SS:       :%8X8:S88888t88;                                                                                                                       
\027[0m                                              \x1B[38;2;178;0;0m     t;@@888        SS8@:@8888SS ::                                                                                                                       
\027[0m                                              \x1B[38;2;190;0;0m      888X8@        S88@8X88X8 8:                                                                                                                         
\027[0m                                              \x1B[38;2;210;0;0m      %88@88S       @888t8X8:8:;                                                                                                                          
\027[0m\n";
  print_string "\x1B[4;10H\x1B[38;2;255;0;0mThe goal of this game is to spread your disease and kill as many people as possible.";
  print_string "\x1B[5;10H\x1B[38;2;255;0;0mUpgrade your disease so it can spread across water, roads, and through civilizations.";
  print_string "\x1B[6;10H\x1B[38;2;255;255;255mName your disease:";
  print_string "\x1B[7;10H\x1B[38;2;255;255;255m> ";
  let disease_name = read_line () in
  print_string "\x1B[9;10H\x1B[38;2;255;255;255mWhile the game is running, press any key to pause or enter commands.";
  print_string "\x1B[10;10H\x1B[38;2;255;255;255mPress any key to start!";
  read_line ();
  start_game state coords




(** [setup_disease] lets the user initialize the map with a civilization
    and choose different values for the disease they want to place into the
    world. *)
let setup_game =
  ANSITerminal.resize 200 45;
  print_string "\x1Bc";
  print_string 
    "\n\n\n\n\x1B[38;2;179;137;179m
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
    setup_disease state "10 10"
  else if selection = "b" || selection = "rabies" then
    let state = {temp_state with disease = Objects.rabies_default} in
    setup_disease state "10 10"
  else if selection = "c" || selection = "cooties" then
    setup_disease temp_state "10 10"

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
     setup_disease state starting_coordinates)


(** [main ()] starts the game and prompts the user for the starting coordinates
    of the disease. *)
let main () = 
  setup_game

let () = main ()