open Pervasives
open Objects

type command = 
  | Quit
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
  | [] -> raise Disease_Malformed
  | h::t -> 
    if h = "exit" then disease else
    if h = "1" then {disease with inner_tile_spread = lst_to_int t} else
    if h = "2" then {disease with tile_to_tile_spread = lst_to_int t} else
    if h = "3" then {disease with civ_to_civ_spread = lst_to_int t} else
    if h = "4" then {disease with spread_probability = lst_to_int t} else
    if h = "5" then {disease with lethality = lst_to_int t} else
      raise Disease_Malformed

let parse (str : string) =
  let command_list = (String.split_on_char ' ' str) in 
  let filtered_list = List.filter (fun ele -> ele <> "") command_list in
  match filtered_list with
  | [] -> raise Empty
  | h::t ->      
    if h = "quit" then Quit else 
    if h = "disease" then Disease else raise Malformed

let print_disease_menu (disease : Disease.t) =
  print_string "\x1Bc"; 
  print_string "Current disease stats:\n1. Inner tile spread: ";
  print_endline (string_of_int disease.inner_tile_spread);
  print_string "2. Tile to tile spread: ";
  print_endline (string_of_int disease.tile_to_tile_spread);
  print_string "3. Civilization to civilization spread: ";
  print_endline (string_of_int disease.civ_to_civ_spread);
  print_string "4. Spread probability: ";
  print_endline (string_of_int disease.spread_probability);
  print_string "5. Lethality: ";
  print_endline (string_of_int disease.lethality);
  print_endline "If you would like to change anything, type the stat number followed by the new value ([1 40] changes the inner tile spread to 40. If you don't, type [exit]."
