(** Represents the disease the user places into the world. *)
module Disease = struct
  (** Tracks information about the disease's spread probability within a tile,
      between tiles, between civilizations, and chance of tile-to-tile spread.*)
  type t = {
    inner_tile_spread : int;
    tile_to_tile_spread : int;
    civ_to_civ_spread : int;
    spread_probability : int;
  }
end 

(** Represents a civilization that contains civilization tiles. *)
module Civilization = struct
  (** Tracks a civilization's total information: total number of infected
      people, total population, and neighboring civilizations. *)
  type t = {
    infected : int ref;
    population : int;
    neighbors : (t ref) list;
  }
end

(** Represents a tile in the world/map. *)
module Tile = struct
  (** The type of the tile *)
  type tile_types = Civ of Civilization.t | Land | Water | Road
  (** Tracks information about the tile, like its type, number of people
      infected in the tile, and the total number of people in the tile.*)
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