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
  let civ1 = Civilization.{infected = ref 0; population= 2500; neighbors= []} in
  let map = Array.make_matrix 5 5 
      Tile.{tile_type = (Civ civ1); infected = 0; population = 100} in
  let disease = Disease.{inner_tile_spread = 5; 
                         tile_to_tile_spread = 60; civ_to_civ_spread = 0} in
  State.{civilizations = [civ1]; disease = disease; tiles = map; elapsed_time = 0}

let second_test_state =
  let civ2 = Civilization.{infected = ref 2500; population= 2500; neighbors= []} in
  let map = Array.make_matrix 5 5 
      Tile.{tile_type = (Civ civ2); infected = 100; population = 100} in
  let disease = Disease.{inner_tile_spread = 10; 
                         tile_to_tile_spread = 60; civ_to_civ_spread = 0} in
  State.{civilizations = [civ2]; disease = disease; tiles = map; elapsed_time = 0}