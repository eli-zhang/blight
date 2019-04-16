open Pervasives

type command = 
  | Start of int list
  | Help
  | Quit

exception Empty
exception Malformed

let parse str =
  let command_list = (String.split_on_char ' ' str) in 
  let filtered_list = List.filter (fun ele -> ele <> "") command_list in
  match filtered_list with
  | [] -> raise Empty
  | h::t ->
    if h = "start" then if List.length(t) <> 2 then raise Malformed 
      else Start (List.map int_of_string t)
    else if h = "help" then if t = [] then Help else raise Malformed             
    else if h = "quit" then if t = [] then Quit else raise Malformed
    else raise Malformed
