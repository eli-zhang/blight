open Objects

let random_check threshold =
  (Random.int 100) < threshold

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
  | Water percentage -> if percentage = 0 then {tile with tile_type = Water 1}
    else tile;
  | Road percentage -> if percentage = 0 then {tile with tile_type = Road 1}
    else tile;
  | _ -> tile

(** [infect_tile tile disease] spreads the infection in tile [tile] if it 
    contains any infected people (increases infected number). If [tile] is not 
    infected, it does nothing. *)
let infect_tile (tile : Tile.t) (disease : Disease.t) : Tile.t =
  match tile.tile_type with
  | Civ civ -> if tile.infected = 0 then tile else 
      let new_infected = int_of_float 
          (floor (((float_of_int disease.inner_tile_spread)
                   *. float_of_int tile.living) /. 100.0 +. 0.5))
                         + tile.infected 
                         + if disease.inner_tile_spread > 0 then 1 else 0  in 
      let died = if random_check disease.lethality
        then (let max_dead = max (if disease.lethality > 0 then 1 else 0) 
                  (int_of_float (floor (((float_of_int tile.living) *.  
                                         (float_of_int disease.lethality) /. 
                                         100.0) +. 0.5))) in
              let died_count = min (tile.infected - tile.dead) 
                  (min max_dead tile.living) in
              civ.living := (!(civ.living) - died_count);
              civ.dead := (!(civ.dead) + died_count);
              died_count)
        else 0 in

      if new_infected > tile.population 
      then (civ.infected := !(civ.infected) + tile.population - tile.infected;
            {tile with infected = tile.population; 
                       living = tile.living - died;
                       dead = tile.dead + died})
      else (civ.infected := !(civ.infected) + new_infected - tile.infected;
            {tile with infected = new_infected;
                       living = tile.living - died;
                       dead = tile.dead + died})
  | Water percentage -> if percentage > 0
    then {tile with tile_type = 
                      Water (min (percentage + disease.inner_tile_spread) 100)}
    else tile
  | Road percentage -> if percentage > 0
    then {tile with tile_type = 
                      Road (min (percentage + disease.inner_tile_spread) 100)}
    else tile
  | _ -> tile

let spread_to_neighbors tiles row column rows cols (disease: Disease.t) =
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

(** [check_neighbors tiles row column disease] checks if a tile at a given [row]
    and [column] in the map [tiles] is over the disease [disease]'s infection 
    threshold for spreading to neighboring tiles. If it is, the function 
    randomly spreads to neighboring tiles based 
    on the disease's random spread probability. *)
let check_neighbors (tiles: Tile.t array array) row column (disease: Disease.t) =
  let tile = tiles.(row).(column) in
  let rows = Array.length tiles - 1 in
  let cols = Array.length tiles.(row) - 1 in

  match tile.tile_type with
  | Civ total_infected -> if tile.infected > 0 then
      if 100 * tile.infected / tile.population >= disease.tile_to_tile_spread then
        if tile.living > 0 then
          spread_to_neighbors tiles row column rows cols disease
  | Water percentage -> if percentage > 0 then
      if percentage >= disease.water_spread then
        spread_to_neighbors tiles row column rows cols disease
  | Road percentage -> if percentage > 0 then
      if percentage >= disease.road_spread then
        spread_to_neighbors tiles row column rows cols disease
  | _ -> ()