(** Represents the disease the user places into the world. *)
module Disease = struct
  (** Tracks information about the disease's spread probability within a tile,
      between tiles, between civilizations, and chance of tile-to-tile spread.*)
  type t = {
    inner_tile_spread : int;
    tile_to_tile_spread : int;
    water_spread: int;
    road_spread: int;
    spread_probability : int;
    lethality : int;
  }
end 

(** Represents a civilization that contains civilization tiles. *)
module Civilization = struct
  (** Tracks a civilization's total information: total number of infected
      people, total population, and neighboring civilizations. *)
  type t = {
    infected : int ref;
    living: int ref;
    dead: int ref;
    population : int;
    neighbors : (t ref) list;
  }
end

(** Represents a tile in the world/map. *)
module Tile = struct
  (** The type of the tile *)
  type tile_types = Civ of Civilization.t | Land | Water of int | Road of int
  (** Tracks information about the tile, like its type, number of people
      infected in the tile, and the total number of people in the tile.*)
  type t = {
    tile_type : tile_types;
    infected : int;
    living: int;
    dead: int;
    population : int;
  }
end

let ebola_default = Disease.{inner_tile_spread = 5; 
                             tile_to_tile_spread = 40;
                             water_spread = 80;
                             road_spread = 20;
                             spread_probability = 50;
                             lethality = 60}

let rabies_default = Disease.{inner_tile_spread = 10; 
                              tile_to_tile_spread = 70;
                              water_spread = 0;
                              road_spread = 10;
                              spread_probability = 100;
                              lethality = 80}

let cooties_default = Disease.{inner_tile_spread = 40; 
                               tile_to_tile_spread = 0;
                               water_spread = 0;
                               road_spread = 100;
                               spread_probability = 10;
                               lethality = 0}