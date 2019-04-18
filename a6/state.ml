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
  let civ1 = Civilization.{infected = ref 0; population = 80000; neighbors= []} in
  let map = Array.make_matrix 20 40
      Tile.{tile_type = (Civ civ1); infected = 0; population = 100} in
  let disease = Disease.{inner_tile_spread = 5; 
                         tile_to_tile_spread = 80; 
                         civ_to_civ_spread = 0;
                         spread_probability = 30} in
  State.{civilizations = [civ1]; disease = disease; tiles = map; elapsed_time = 0}

let advanced_starting_state = 
  let height = 20 in
  let length = 40 in
  let civ1 = Civilization.{infected = ref 0; population = 0; neighbors = []} in
  let civ2 = Civilization.{infected = ref 0; population = 0; neighbors = []} in
  let map = Array.make_matrix height length
      Tile.{tile_type = Land; infected = 0; population = 0} in
  (for x = (height / 4) to (height / 3) do
     for y = (length / 4) to (length / 3) do
       map.(x).(y) <- 
         Tile.{tile_type = (Civ civ1); infected = 0; population = 100};
     done
   done);
  (for x = (height / 5) to (height / 6) do
     for y = (length / 4) to (length / 3) do
       map.(x).(y) <-
         Tile.{tile_type = (Civ civ2); infected = 0; population = 100};
     done
   done);

  let disease = Disease.{inner_tile_spread = 5; 
                         tile_to_tile_spread = 80; 
                         civ_to_civ_spread = 0;
                         spread_probability = 30} in
  State.{civilizations = [civ1]; disease = disease; tiles = map; elapsed_time = 0}