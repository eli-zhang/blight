module Civilization = struct
  type t = {
    infected : int;
    population : int;
    neighbors : int list;
  }
end 

module Disease = struct
  type t = {
    inner_tile_spread : int;
    tile_to_tile_spread : int;
    civ_to_civ_spread : int;
  }
end 

module Tile = struct
  type t = Civ of Civilization.t | Land | Water | Road
end