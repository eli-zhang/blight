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
  match tile.tile_type with
  | Civ total_infected -> if tile.infected > 0 && tile.infected < tile.population then
      if 100 * tile.infected / tile.population > disease.tile_to_tile_spread then
        begin
          if row > 0 && column > 0 then
            tiles.(row - 1).(column - 1) <- startTileInfection (tiles.(row - 1).(column - 1));
          if row > 0 then
            tiles.(row - 1).(column) <- startTileInfection (tiles.(row - 1).(column));
          if row > 0 && column < 5 then
            tiles.(row - 1).(column + 1) <- startTileInfection (tiles.(row - 1).(column + 1));
          if (column > 0) then
            tiles.(row).(column - 1) <- startTileInfection (tiles.(row).(column - 1));
          if (column < 5) then
            tiles.(row).(column + 1) <- startTileInfection (tiles.(row).(column + 1));
          if (row < 5) then
            tiles.(row + 1).(column) <- startTileInfection (tiles.(row + 1).(column));
          if row < 5 && column < 5 then
            tiles.(row + 1).(column + 1) <- startTileInfection (tiles.(row + 1).(column + 1));
          if row < 5 && column > 0 then
            tiles.(row + 1).(column - 1) <- startTileInfection (tiles.(row + 1).(column - 1));
        end	
  | _ -> ()

let infectMap (map : Tile.t array array) (xy: int * int) = 
  startTileInfection map.(fst xy).(snd xy)
