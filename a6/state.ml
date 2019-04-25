open Objects

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

let total_dead (state: State.t) =
  List.fold_left (fun acc (civ: Civilization.t) -> 
      acc + !(civ.dead)) 0 state.civilizations

let total_infected (state: State.t) = 
  List.fold_left (fun acc (civ: Civilization.t) -> 
      acc + !(civ.infected)) 0 state.civilizations

let total_population (state: State.t) =
  List.fold_left (fun acc (civ: Civilization.t) -> acc + civ.population) 
    0 state.civilizations