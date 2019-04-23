open Objects

module State = struct
  type t = {
    civilizations: Civilization.t list;
    disease: Disease.t;
    tiles: Tile.t array array;
    elapsed_time: int;
  }
end