open Objects

(** Tracks the current state of the world. *)
module State = struct
  type t = {
    civilizations: Civilization.t list;
    disease: Disease.t;
    name: string;
    upgrades: string list;
    tiles: Tile.t array array;
    elapsed_time: int;
    civcoords: (int*int) array;
    news_message: string
  }
end

(** [total_dead state] calculates the total number of dead people in the world
    given by the state [state]. *)
let total_dead (state: State.t) =
  List.fold_left (fun acc (civ: Civilization.t) -> 
      acc + !(civ.dead)) 0 state.civilizations

(** [total_infected] calculates the total number of infected people in 
    the world given by the state [state]. *)
let total_infected (state: State.t) = 
  List.fold_left (fun acc (civ: Civilization.t) -> 
      acc + !(civ.infected)) 0 state.civilizations

(** [total_population] calculates the total population in the world given
    by the state [state]. *)
let total_population (state: State.t) =
  List.fold_left (fun acc (civ: Civilization.t) -> acc + civ.population) 
    0 state.civilizations