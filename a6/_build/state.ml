open Objects

module State = struct
  type t = {
    civilizations: Civilization.t list;
    disease: Disease.t;
    tiles: Tile.t array array;
    elapsed_time: int;
  }
end

let starting_state = 
  let civ1 = Civilization.{infected = ref 0; population= 62500; neighbors= []} in
  let map = Array.make_matrix 20 40
      Tile.{tile_type = (Civ civ1); infected = 0; population = 100} in
  let disease = Disease.{inner_tile_spread = 5; 
                         tile_to_tile_spread = 80; 
                         civ_to_civ_spread = 0;
                         spread_probability = 30} in
  State.{civilizations = [civ1]; disease = disease; tiles = map; elapsed_time = 0}