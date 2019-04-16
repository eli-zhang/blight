open TerminalPrint
open RandomMap
open Controller
open Unix
open Objects
open Infection

let rec run_game (map : Tile.t array) (disease : Disease.t) = 
  failwith("Unimplemented")

let rec start_game (start : Controller.command) =
  failwith("Unimplemented")

let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to our game.\n");
  print_endline "Please enter the coordinates for where to start the disease.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | input -> start_game (parse input)

let () = main()