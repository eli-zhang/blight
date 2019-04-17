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

  (for x = 0 to 4 do
     for y = 0 to 4 do
       check_neighbors tiles x y disease;
       tiles.(x).(y) <- infectTile (tiles.(x).(y))(disease);
     done
   done);
  (* let updated_civilizations = infect_civilizations [] civilizations disease in *)
  Unix.sleepf 0.1;
  run_game {st with elapsed_time = st.elapsed_time + 1}

(* let rec start_game (start : string list) =
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
    then print_error_retry else try run_game starting_state with Too_Big ->
      print_too_big
   with Failure string -> print_error_retry *)


let main () = 
  (* ANSITerminal.(print_string [red]
                  "\n\nWelcome to our game.\n");
     print_string "> ";
     print_string "\027[2J";
     print_string "random print statement"; *)
  run_game starting_state
(* match read_line () with
   | exception End_of_file -> ()
   | input -> (print_string "\027[2J";
            run_game starting_state)
             run_game starting_state *)


let () = main ()