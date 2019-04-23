open Objects

module State = struct
  type t = {
    civilizations: Civilization.t list;
    disease: Disease.t;
    tiles: Tile.tile_types array array;
    elapsed_time: int;
    civcoords: (int*int) array
  }
end