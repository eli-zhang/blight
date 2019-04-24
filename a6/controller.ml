open Pervasives
open Objects

type command =
  | Quit
  | Continue
  | Disease

exception Empty
exception Malformed
exception Disease_Malformed

let disease_parse (str : string) (disease : Disease.t) =
  let command_list = (String.split_on_char ' ' str) in
  let filtered_list = List.filter (fun ele -> ele <> "") command_list in
  let lst_to_int lst = if List.length lst <> 1 then raise Disease_Malformed else
      try int_of_string (List.hd lst)
      with Failure x -> raise Disease_Malformed in
  match filtered_list with
  | [] -> disease
  | h::t ->
    if h = "exit" then raise Disease_Malformed else
    if h = "1" then {disease with inner_tile_spread = lst_to_int t} else
    if h = "2" then {disease with tile_to_tile_spread = lst_to_int t} else
    if h = "3" then {disease with spread_probability = lst_to_int t} else
    if h = "4" then {disease with lethality = lst_to_int t} else
      raise Disease_Malformed

let parse (str : string) =
  let command_list = (String.split_on_char ' ' str) in
  let filtered_list = List.filter (fun ele -> ele <> "") command_list in
  match filtered_list with
  | [] -> raise Empty
  | h::t ->
    if h = "quit" then Quit else
    if h = "continue" then Continue else
    if h = "disease" then Disease else raise Malformed

let print_disease_menu_numbers (disease : Disease.t) =
  print_string "Current disease stats:\n1. Inner tile spread: ";
  print_endline (string_of_int disease.inner_tile_spread);
  print_string "2. Tile to tile spread: ";
  print_endline (string_of_int disease.tile_to_tile_spread);
  print_string "3. Spread probability: ";
  print_endline (string_of_int disease.spread_probability);
  print_string "4. Lethality: ";
  print_endline (string_of_int disease.lethality);
  print_endline "If you would like to change anything, 
  type the stat number followed by the new value 
  ([1 40] changes the inner tile spread to 40.";
  try disease_parse (read_line ()) disease
  with Disease_Malformed -> disease

let print_disease_menu (disease : Disease.t) =
  let rec print_bar_helper percent color =
    let threshold = 5 in
    if percent >= threshold 
    then (print_string (color ^ "  ");
          print_bar_helper (percent - 5) color); in
  print_endline "1. Infectivity:";
  print_bar_helper disease.inner_tile_spread "\x1B[48;2;0;58;0m";
  print_bar_helper (100 - disease.inner_tile_spread) "\x1B[48;2;255;255;255m";
  print_endline "\027[0m\n";
  print_endline "2. Transmission Rate:";
  print_bar_helper disease.spread_probability "\x1B[48;2;0;58;0m";
  print_bar_helper (100 - disease.spread_probability) "\x1B[48;2;255;255;255m";
  print_endline "\027[0m\n";
  print_endline "3. Incidence:";
  print_bar_helper disease.tile_to_tile_spread "\x1B[48;2;0;58;0m";
  print_bar_helper (100 - disease.tile_to_tile_spread) "\x1B[48;2;255;255;255m";
  print_endline "\027[0m\n";
  print_endline "4. Virulence:";
  print_bar_helper disease.lethality "\x1B[48;2;200;0;0m";
  print_bar_helper (100 - disease.lethality) "\x1B[48;2;255;255;255m";
  print_endline "\027[0m\n";
  print_endline "If you would like to change anything, 
  type the stat number followed by the new value 
  ([1 40] changes the Infectivity to 40.)";
  try disease_parse (read_line ()) disease
  with Disease_Malformed -> disease