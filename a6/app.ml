open TerminalPrint
open RandomMap
open Controller
open Unix
open Objects
open Infection
open State

let string_to_list str =
  let command_list = (String.split_on_char ' ' str) in 
  List.filter (fun ele -> ele <> "") command_list

(* let rec run_game (st: State.state) =
   let civilizations = state.civilizations in
   let disease = state.disease in
   let tiles = state.tiles in

   for x = 0 to 100 do
    for y = 0 to 100 do
      tiles.(x).(y) := InfectTile(tiles.(x).(y))(disease);
      check_neighbors tiles x y disease;
    done
   done
   let updated_civilizations = infect_civilizations [] civilizations disease in
   run_game (state with civilizations = updated_civilizations) *)


let rec start_game (start : string list) =
  let print_error_retry = ANSITerminal.(print_string [red] "You need to input two numbers!\n"); 
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | input -> start_game (string_to_list input) in
  let print_too_big = ANSITerminal.(print_string [red] "The coordinates are over the map!\n"); 
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | input -> start_game (string_to_list input) in
  try let xy = List.map int_of_string start in if List.length start <> 2 
    then print_error_retry else try run_game (*generate map*) with Too_Big ->
      print_too_big
  with Failure string -> print_error_retry


let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to our game.\n");
  print_endline "Please enter the coordinates for where to start the disease.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | input -> start_game (string_to_list input)

let () = main()