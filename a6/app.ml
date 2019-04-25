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
    |  exception Empty -> print_endline "You need to input something! Press any key to continue.";
      (match input_char Pervasives.stdin with
       | _ -> st)
    | exception Malformed -> print_endline "That's not a valid command! Press any key to continue."; 
      (match input_char Pervasives.stdin with
       | _ -> st)
    | Disease -> let st' = {st with disease=print_disease_menu st.disease} in
      print_endline "Enter another command; type \"continue\" to continue.";
      read_command st';
    | Continue -> st;
    | Quit -> print_string "Thanks for playing!";
      exit 0;
  end

(** [run_game st] is the main game loop that steps the game state [st],
    spreads disease within and between tiles, and prints out the map. *)
let rec run_game (st: State.t) =
  Unix.sleepf 0.1;
  print_endline "Type [disease] to view the current disease stats and change them if needed. Type [quit] to quit the game.";
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
      print_string "\n";
      TerminalPrint.print_world_info st;
      print_endline "Press any key to pause the game or enter a command.";
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

let setup_disease (state: State.t) =
  print_string "\x1Bc
\027[0m                                              \x1B[38;2;51;0;0m                                                                          :%SSttXX31t 311 3110311@%X t            X@S 8          XS8S8X     ;S8SX311@:    
\027[0m                                              \x1B[38;2;70;0;0m                                                                      :8%@S8S31108XS@ 8t8%;;tt ;8 8X31;          X8@;31;:        8X t@8:   t8Xt3110X      
\027[0m                                              \x1B[38;2;91;0;0m                                                                     %XS8S8 8 :311S8 tS:         3110@S          S 8@X8:         @8;%XX   @@8X 8X31:      
\027[0m                                              \x1B[38;2;111;0;0m                                                                   :%8@8X311X;31S31 :            S31108          ;8@8SXX:        8XS31   @X31 311 ;       
\027[0m                                              \x1B[38;2;133;0;0m                                                                  XtX@31108%31SX; 31t@           :%8@8SS          %31X31;        8;8tX  S8%31 8:@         
\027[0m                                              \x1B[38;2;144;0;0m                                                                ;XXXXX311;3110t tt8tX :            8S8@8:         :@8S31X        SX8 8:8@8X31108          
\027[0m                                              \x1B[38;2;155;0;0m                                                              :@S@X311X31 X@t:   ;8X@8 ;           8;@31:          8;t8X8       ;@8 31@@311@ 31t          
\027[0m                                              \x1B[38;2;166;0;0m                                                             %@31@@S SX8;t        @%8S@%%           @ X31@        : X8S:8:       S 8 31X311;8X:           
\027[0m                                              \x1B[38;2;173;0;0m                                                           S311X8;S@XS8S;          8@3110           ;8%31 :         3110%       : :X8X8S@311;             
\027[0m                                              \x1B[38;2;180;0;0m                                                         :tS%@8X8@%3110t           X @31@8t         ;@S31X8         t;;SX@       %31S311 @ S              
\027[0m                                              \x1B[38;2;200;0;0m                                                       :@S311XX8 S31108S@           ;31;8X%;         :8S 8St         31108:      8S:@ ;@:31               
\027[0m                                              \x1B[38;2;211;0;0m                                                       t311X311St31%31X@ :           ;tX8@@           % S31S%        X  8        8@S8S311X;               
\027[0m                                              \x1B[38;2;223;0;0m                                                     S3110 31@8 S: %311%S@             8X @tt          ;3110 ;        31S;8:   :%@tt@8@X 8                
\027[0m                                              \x1B[38;2;211;0;0m                                                    t8@311;@S ;;    :S 311@X           %31%8X8         :8XX%31        31@@XS  t3110  ::8t                 
\027[0m                                              \x1B[38;2;200;0;0m                                                  :X311@8@X8X8;       ;%@8;@:%          S8t 8@;         X 8X8         ; 31SS  %31108X31t                  
\027[0m                                              \x1B[38;2;180;0;0m                                                 ;8@;3110X8 :t         X8@;31;           X%%31@          tX8:X%        8@S31 8S:8% 8  8                   
\027[0m                                              \x1B[38;2;170;0;0m                                                S@S@31%8t 8X            ;;t@8XX8          X@31X%S        ::@31X%      ;311%311 8S8%%S                     
\027[0m                                              \x1B[38;2;160;0;0m                                              :SX@31%@S@S@ S              %:; 8;          :311SXSt        X@8S31;     :8S31%@@311%8;                      
\027[0m                                              \x1B[38;2;155;0;0m                                             ;XSS311@31  8S:S8@            311X8SX         :St8XS8        ;X;X31X      @ 31S8@X311%                       
\027[0m                                              \x1B[38;2;144;0;0m    ::;;;:%%S%;:  :tt:                       8@3110S311@X311 8 ;:          :S3110;8X:        8X311Xt       : 3110@    SStXS8%t@ 31                        
\027[0m                                              \x1B[38;2;120;0;0mS;X@XS31XX3110X%31%8@8XX@t;SSt;:             St@311 8S;  :8X 3110t           :SX8@31@        ;31X8t8:       X 311X;  t31 8S8X  8X                         
\027[0m                                              \x1B[38;2;111;0;0mS@X@S311S8SS311%S@31@X8@X311@8@;@3110SSt;:;8S31X@S%8;     S31:8X 31;          :8  :8;        :@%XS8@8;       % 8@ 311S8 @8S8X 8%                          
\027[0m                                              \x1B[38;2;104;0;0m8@X311XXSX3110XX8@%31%311031@@%8@%8X31108@@8 :SXS X X        @311:31 8@        ;%311S 8         @ X31SS       8  8Xt31%@8 8 31;                           
\027[0m                                              \x1B[38;2;154;0;0m@S@8XX@3110311031103110@311%31108tS3110311S311031;8tSt8S@8X8% ;S8SX8X8S8         @: @S 8:        3110%8:      @S@t; 31tX:@%@;                             
\027[0m                                              \x1B[38;2;180;0;0mX311031@311;SX@3110311@t;XX31@3110311@311031X8S311@311031 X8 8XX311tt8X@S;:       :SS%8@ ;        31:@8S @@@XX8X31t@X%S X 8t                              
\027[0m                                              \x1B[38;2;220;0;0m3110 31108S8%3110%@3110@8@311:31108%  SS::XSSSX8@@@311@8SS%@@;t31@ 8S   31S;@;@%8S8SXX@@@Xt::   : 8%31 XXXX;311;8@ 31:311;:                               
\027[0m                                              \x1B[38;2;255;0;0m31@8Xtt;;       ;S31108X :;;::%t;tX31108StX8%S@@X8SX3110@31108X8S311031 31S3110S@@31%8t31%3110@S8@XS %S 31 8t%tX311t311;:                                 
\027[0m                                              \x1B[38;2;215;0;0m31108            8XS8@ :          31108S8S8@ 3110311 t:%t8XX31@XX31108X3110X@8@8X XX8S%8 StX3110X8X@8StX31% 8SSS@X@8S@t                                   
\027[0m                                              \x1B[38;2;180;0;0m SXS31           t 31@ :         SX%311X8X8 X::;;;;t    %31@31108 8tX@XX@8@311S31S @8@S31X 8XX::8 SX311 @X @8t31%8;                                       
\027[0m                                              \x1B[38;2;150;0;0m31108:;          ;X@X@X8        :311031@31@:                   :;:tS8%31%%S8X % @8%t@8 @8tS8 S8;31:311 X8 X @%@:t:                                        
\027[0m                                              \x1B[38;2;125;0;0m3110311          tt@31@8:       t31X8 XSXtS                         ::t;   ;St;X:t31S%8;8S: 8XXS 8t@X @8X31t:                                             
\027[0m                                              \x1B[38;2;100;0;0m;;X8@%:            8@8 8       t@3110;:@8:                                            ;    ;SX::::%St                                                     
\027[0m                                              \x1B[38;2;105;0;0m %X@31@t          S8S8@ :     %X8%3110 S                                                      :                                                           
\027[0m                                              \x1B[38;2;110;0;0m ;S311X%          St31108   :tSt8% :31%X                                                                                                                  
\027[0m                                              \x1B[38;2;125;0;0m  tt311@X         X @8XXX   S311tS8@ @t                                                                                                                   
\027[0m                                              \x1B[38;2;135;0;0m   t31108         %;X %31% tS 31X311S :                                                                                                                   
\027[0m                                              \x1B[38;2;143;0;0m   ;t8@31X         ;t@31 S8S8@311XSt;                                                                                                                     
\027[0m                                              \x1B[38;2;158;0;0m     @8@8 t        :XS@8%%X8t@31108:                                                                                                                      
\027[0m                                              \x1B[38;2;165;0;0m     @S31SS:       :%8X8:S31108t31;                                                                                                                       
\027[0m                                              \x1B[38;2;178;0;0m     t;@@311        SS8@:@3110SS ::                                                                                                                       
\027[0m                                              \x1B[38;2;190;0;0m      311X8@        S31@8X31X8 8:                                                                                                                         
\027[0m                                              \x1B[38;2;210;0;0m      %31@31S       @311t8X8:8:;                                                                                                                          
\027[0m\n";
  print_string "\x1B[4;10H\x1B[38;2;255;0;0mThe goal of this game is to spread your disease and kill as many people as possible.";
  print_string "\x1B[5;10H\x1B[38;2;255;255;255mName your disease:";
  print_string "\x1B[6;10H\x1B[38;2;255;255;255m> ";
  let disease_name = read_line () in
  print_string ("\x1B[7;10H\x1B[38;2;0;200;30mIn order to win, " ^ disease_name ^ " has to wipe out the world.");
  print_string "\x1B[8;10H\x1B[38;2;255;0;0mUpgrade your disease so it can spread across water, roads, and through civilizations.";
  print_string "\x1B[9;10H\x1B[38;2;255;255;255mWhile the game is running, press any key to pause or enter commands.";
  print_string "\x1B[10;10H";

  let disease : Disease.t = state.disease in
  let rec print_bar_helper percent color =
    let threshold = 5 in
    if percent >= threshold 
    then (print_string (color ^ " ");
          print_bar_helper (percent - threshold) color); in
  print_string "\027[1m\x1B[22;10HDisease Information:\027[0m";
  print_string "\x1B[24;10H1. Infectivity:";
  print_string "\x1B[25;10H";
  print_bar_helper disease.inner_tile_spread "\x1B[48;2;0;58;0m";
  print_bar_helper (100 - disease.inner_tile_spread) "\x1B[48;2;160;160;160m";
  print_string "\027[0m\n";
  print_string "\x1B[27;10H2. Transmission Rate:";
  print_string "\x1B[28;10H";
  print_bar_helper disease.spread_probability "\x1B[48;2;0;58;0m";
  print_bar_helper (100 - disease.spread_probability) "\x1B[48;2;160;160;160m";
  print_string "\027[0m\n";
  print_string "\x1B[30;10H3. Incidence:";
  print_string "\x1B[31;10H";
  print_bar_helper disease.tile_to_tile_spread "\x1B[48;2;0;58;0m";
  print_bar_helper (100 - disease.tile_to_tile_spread) "\x1B[48;2;160;160;160m";
  print_string "\027[0m\n";
  print_string "\x1B[33;10H4. Virulence:";
  print_string "\x1B[34;10H";
  print_bar_helper disease.lethality "\x1B[48;2;200;0;0m";
  print_bar_helper (100 - disease.lethality) "\x1B[48;2;160;160;160m";
  print_string "\027[0m\n";
  print_string "\x1B[36;10H\x1B[38;2;255;255;255mPress [Enter] to start!";
  ignore(read_line ());

  print_string "\x1Bc";
  TerminalPrint.print_map state.tiles state.elapsed_time;
  print_endline "\027[0m\nPick starting coordinates in the form \"x y\".";
  print_string "> ";

  start_game {state with name = disease_name} (read_line ())

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
           name = "";
           tiles = map; 
           elapsed_time = 0;
           civcoords = civcoord;
           news_message = ""} in
  if selection = "a" || selection = "ebola" then
    let state = {temp_state with disease = Objects.ebola_default} in
    setup_disease state
  else if selection = "b" || selection = "rabies" then
    let state = {temp_state with disease = Objects.rabies_default} in
    setup_disease state
  else if selection = "c" || selection = "cooties" then
    setup_disease temp_state

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
                       neighbors = []} in

       let disease = Disease.{inner_tile_spread = inner_tile_spread; 
                              tile_to_tile_spread = tile_to_tile_spread;
                              water_spread = 50;
                              road_spread = 30;
                              spread_probability = spread_probability;
                              lethality = lethality} in
       State.{civilizations = [civ1]; 
              disease = disease; 
              name = "";
              tiles = map; 
              elapsed_time = 0;
              civcoords = civcoord;
              news_message = ""} in
     generateMap state.tiles state.civcoords 10;
     setup_disease state)

(** [main ()] starts the game and prompts the user for the starting coordinates
    of the disease. *)
let main () = 
  setup_game

let () = main ()