#require Civilization
open Tile
open TerminalPrint
open RandomMap
open Controller
open Disease
open Unix
open Thread
open Civilization

module state = struct
  Tile list
    Civilization list
    Disease disease
    InfectTile
    InfectNeighbors
    InfectCivilizations

  (* [run_game state] runs the game until the player says quit or wins *)
  let rec run_game state =
    InfectTile (tile);
    InfectNeighbors(tile);
    InfectCivilization (Civilization []);
    ignore(Thread.create (run_game state) 1);
    match (parse (read_line stdin)) with
    | exception Malformed -> 
      ANSITerminal.(print_string [red] 
                      "Unrecognized command. Type \"help\" for help. \n");  
      run_game map 
    | exception Empty -> ignore(printl "You have to enter a command! \n");  
      run_game map;
    | Start int1*int2 
      -> printf "You're disease already started!";  
      (Thread.create (run_game map) 1);
    | Help -> ANSITerminal.(printl [blue] "\n
  Available commands
  --------------------
  • Start _______
  • Pause
  • Help 
  • Quit\n\n\n");
      run_game map
    | Quit -> ignore(printl "Better luck next time!"); exit 0 

  (** [start ()] starts the game. *)
  let start () =
    match (parse (read_line stdin)) with
    | exception Malformed -> 
      ANSITerminal.(print_string [red] 
                      "Unrecognized command. Type \"help\" for help. \n");  
      start ()
    | exception Empty 
      -> ignore(printl "You have to enter a command! \n"); start ();
    | Start int1*int2 
      -> diseasestuff int1 int2; (Thread.create (run_game map) 1)
    | Help -> ANSITerminal.(printl [blue] "\n
  Available commands
  --------------------
  • Start _______
  • Pause
  • Help 
  • Quit\n\n\n"); start ();
    | Quit -> ignore(printl "Better luck next time!"); exit 0


  (* Execute the game engine. *)
  let main ()
      print_world world;
    ANSITerminal.(printl [red] "\n\nWelcome to our game!  Start your plaque.\n");
    start ();;

  (* Execute the game engine. *)
  let () = main ();


