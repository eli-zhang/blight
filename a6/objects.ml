module Disease = struct
  type t = {
    inner_tile_spread : int;
    tile_to_tile_spread : int;
    civ_to_civ_spread : int;
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
  type t = Civ of Civilization.t | Land | Water | Road
end

(* let rec infect_civilizations 
    (civilizations: civilization list) (disease: disease) =
   match civilizations with
   | [] -> ()
   | (civ::t) -> 
    let infected = civ.total_infected in
    let population = civ.total_population in
    if (!infected / population * 100) > disease.civ_to_civ_spread 
    then let rec infect_neighbors neighbors =
           match neighbors with
           | [] -> ();
           | (civ, tile)::t -> if !(civ.total_infected) = 0 
             then civ.total_infected := 1;
             tile.infected := 1; 
             infect_neighbors t in
      infect_neighbors civ.neighbors;
      infect_civilizations t disease *)