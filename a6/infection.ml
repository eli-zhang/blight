open Objects

let startTileInfection (tile : Tile.t): Tile.t =
  match tile.tile_type with
  | Civ civ -> if !(civ.infected) = 0 
    then (civ.infected := 1; {tile with infected = 1})
    else if tile.infected = 0 then {tile with infected = 1}
    else tile
  | _ -> tile

let infectTile (tile : Tile.t) (disease : Disease.t) : Tile.t =
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

let check_neighbors (tiles: Tile.t array array) row column (disease: Disease.t) =
  let tile = tiles.(row).(column) in
  let rows = Array.length tiles - 1 in
  let cols = Array.length tiles.(0) - 1 in

  let random_check threshold =
    (Random.int 100) + 1 < threshold in

  match tile.tile_type with
  | Civ total_infected -> if tile.infected > 0 && tile.infected < tile.population then
      if 100 * tile.infected / tile.population > disease.tile_to_tile_spread then
        begin
          print_endline ("Row: " ^ string_of_int row);
          print_endline ("Column: " ^ string_of_int column);
          if row > 0 && column > 0 && random_check disease.spread_probability then
            tiles.(row - 1).(column - 1) <- startTileInfection (tiles.(row - 1).(column - 1));
          if row > 0 && random_check disease.spread_probability then
            tiles.(row - 1).(column) <- startTileInfection (tiles.(row - 1).(column));
          if row > 0 && column < cols && random_check disease.spread_probability then
            tiles.(row - 1).(column + 1) <- startTileInfection (tiles.(row - 1).(column + 1));
          if (column > 0) && random_check disease.spread_probability then
            tiles.(row).(column - 1) <- startTileInfection (tiles.(row).(column - 1));
          if (column < cols) && random_check disease.spread_probability then
            tiles.(row).(column + 1) <- startTileInfection (tiles.(row).(column + 1));
          if (row < rows) && random_check disease.spread_probability then
            tiles.(row + 1).(column) <- startTileInfection (tiles.(row + 1).(column));
          if row < rows && column < cols && random_check disease.spread_probability then
            tiles.(row + 1).(column + 1) <- startTileInfection (tiles.(row + 1).(column + 1));
          if row < rows && column > 0 && random_check disease.spread_probability then
            tiles.(row + 1).(column - 1) <- startTileInfection (tiles.(row + 1).(column - 1));
        end	
  | _ -> ()

let infectMap (map : Tile.t array array) (xy: int * int) = 
  startTileInfection map.(fst xy).(snd xy)
