type place = int*int

type command = 
  | Start of place
  (* | Upgrade of disease *)
  (* | Spread of disease *)
  (* | Pause*)
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
    if h = "start" then if t = [] then raise Malformed else Start t
    (*else if h = "upgrade" then if t = [] then raise Malformed else Upgrade t
      else if h = "spread" then if t = [] then raise Malformed else Spread t*)
    (*else if h = "pause" then if t = [] then Pause else raise Malformed *)
    else if h = "help" then if t = [] then Help else raise Malformed             
    else if h = "quit" then if t = [] then Quit else raise Malformed
    else raise Malformed
