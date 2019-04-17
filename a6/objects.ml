module Disease = struct
  type t = {
    inner_tile_spread : int;
    tile_to_tile_spread : int;
    civ_to_civ_spread : int;
    spread_probability : int;
  }
end 

module Civilization = struct
  type t = {
    infected : int ref;
    population : int;
    neighbors : (t ref) list;
  }
end

module Tile = struct
  type tile_types = Civ of Civilization.t | Land | Water | Road
  type t = {
    tile_type : tile_types;
    infected : int;
    population : int;
  }
end

(* let rec infect_civilizations 
    (civilizations: Civilization.t list) (disease: Disease.t) =
   match civilizations with
   | [] -> ()
   | (civ::t) -> 
    let infected = civ.infected in
    let population = civ.population in
    if (!infected / population * 100) > disease.civ_to_civ_spread 
    then let rec infect_neighbors neighbors =
           match neighbors with
           | [] -> ();
           | civ::t -> if !(civ.infected) = 0 
             then civ.infected := 1;
             infect_neighbors t in
      infect_neighbors civ.neighbors;
      infect_civilizations t disease *)