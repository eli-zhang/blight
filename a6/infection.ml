open Objects

(** [start_tile_infection tile] starts an infection in tile [tile] 
    (1 person infected) if it is not infected and does nothing 
    to the tile if it is. It then returns the tile. *)
let start_tile_infection (tile : Tile.t): Tile.t =
  match tile.tile_type with
  | Civ civ -> if !(civ.infected) = 0 
    then (civ.infected := 1; {tile with infected = 1})
    else if tile.infected = 0 then 
      ((civ.infected := !(civ.infected) + 1);
       {tile with infected = 1})
    else tile
  | _ -> tile

(** [infect_tile tile disease] spreads the infection in tile [tile] if it 
    contains any infected people (increases infected number). If [tile] is not 
    infected, it does nothing. *)
let infect_tile (tile : Tile.t) (disease : Disease.t) : Tile.t =
  match tile.tile_type with
  | Civ civ -> if tile.infected = 0 then tile else 
      let new_infected = (disease.inner_tile_spread * tile.population / 100) 
                         + tile.infected in
      if new_infected > tile.population 
      then (civ.infected := !(civ.infected) + tile.population - tile.infected; 
            {tile with infected = tile.population})
      else (civ.infected := !(civ.infected) + new_infected - tile.infected;
            {tile with infected = new_infected})
  | _ -> tile

(** [check_neighbors tiles row column disease] checks if a tile at a given [row]
    and [column] in the map [tiles] is over the disease [disease]'s infection 
    threshold for spreading to neighboring tiles. If it is, the function 
    randomly spreads to neighboring tiles based 
    on the disease's random spread probability. *)
let check_neighbors (tiles: Tile.t array array) row column (disease: Disease.t) =
  let tile = tiles.(row).(column) in
  let rows = Array.length tiles - 1 in
  let cols = Array.length tiles.(row) - 1 in

  let random_check threshold =
    (Random.int 100) + 1 < threshold in

  match tile.tile_type with
  | Civ total_infected -> if tile.infected > 0 then
      if 100 * tile.infected / tile.population >= disease.tile_to_tile_spread then
        begin
          if row > 0 && column > 0 && random_check disease.spread_probability then
            tiles.(row - 1).(column - 1) <- start_tile_infection (tiles.(row - 1).(column - 1));
          if row > 0 && random_check disease.spread_probability then
            tiles.(row - 1).(column) <- start_tile_infection (tiles.(row - 1).(column));
          if row > 0 && column < cols && random_check disease.spread_probability then
            tiles.(row - 1).(column + 1) <- start_tile_infection (tiles.(row - 1).(column + 1));
          if (column > 0) && random_check disease.spread_probability then
            tiles.(row).(column - 1) <- start_tile_infection (tiles.(row).(column - 1));
          if (column < cols) && random_check disease.spread_probability then
            tiles.(row).(column + 1) <- start_tile_infection (tiles.(row).(column + 1));
          if (row < rows) && random_check disease.spread_probability then
            tiles.(row + 1).(column) <- start_tile_infection (tiles.(row + 1).(column));
          if row < rows && column < cols && random_check disease.spread_probability then
            tiles.(row + 1).(column + 1) <- start_tile_infection (tiles.(row + 1).(column + 1));
          if row < rows && column > 0 && random_check disease.spread_probability then
            tiles.(row + 1).(column - 1) <- start_tile_infection (tiles.(row + 1).(column - 1));
        end	
  | _ -> ()