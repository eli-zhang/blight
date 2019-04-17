open TerminalPrint
(* open RandomMap *)
open Controller
open Unix
open Objects
open Infection
open State

let string_to_list str =
  let command_list = (String.split_on_char ' ' str) in 
  List.filter (fun ele -> ele <> "") command_list

let rec run_game (st: State.t) =
  (* let civilizations = st.civilizations in *)
  print_string "\x1Bc";
  TerminalPrint.printMap st.tiles st.elapsed_time;
  flush Pervasives.stdout;
  (* match read_line() with
     | exception End_of_file -> ()
     | input -> *)
  let disease = st.disease in
  let tiles = st.tiles in

  (for x = 0 to Array.length tiles - 1 do
     for y = 0 to Array.length tiles.(x) - 1 do
       check_neighbors tiles x y disease;
       tiles.(x).(y) <- infectTile (tiles.(x).(y))(disease);
     done
   done);
  (* let updated_civilizations = infect_civilizations [] civilizations disease in *)
  Unix.sleepf 0.1;
  run_game {st with elapsed_time = st.elapsed_time + 1}

let rec start_game (start_coordinates : string) =
  (* let print_error_retry = ANSITerminal.(print_string [red] "You need two numbers!\n"); 
     print_string "> ";
     match read_line () with
     | exception End_of_file -> ()
     | input -> start_game input in
     let print_incorrect_format = ANSITerminal.(print_string [red] "Incorrect format!\n"); 
     print_string "> ";
     match read_line () with
     | exception End_of_file -> ()
     | input -> start_game input in *)
  (* let print_too_big = ANSITerminal.(print_string [red] "The coordinates are over the map!\n"); 
     print_string "> ";
     match read_line () with
     | exception End_of_file -> ()
     | input -> start_game input in *)
  let xy = List.map int_of_string (string_to_list start_coordinates) in
  print_endline (string_of_int (List.length xy));
  if List.length xy <> 2 then print_string "zoo" else 
    let x = List.hd xy in
    let y = List.nth xy 1 in
    print_endline (string_of_int x);
    print_endline (string_of_int y);
    let state_with_coordinates = 
      starting_state.tiles.(x).(y) <- startTileInfection starting_state.tiles.(x).(y);
      starting_state in
    run_game state_with_coordinates
(* with Failure string -> print_incorrect_format *)


let main () = 
  print_string "\027[2J";
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to our game.\n");
  print_endline "Enter the starting coordinates in the form \"x y\"";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | input -> start_game input


let () = main ()