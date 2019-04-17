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
  | Civ civ -> if !(civ.infected) = 0 then tile else 
      let new_infected = (disease.inner_tile_spread * civ.population / 100) 
                         + !(civ.infected) in
      if new_infected > civ.population 
      then (civ.infected := civ.population; tile)
      else (civ.infected := new_infected; tile)
  | _ -> tile

let check_neighbors (tiles: Tile.t array array) row column (disease: Disease.t) =
  let tile = tiles.(row).(column) in
  match tile.tile_type with
  | Civ total_infected -> if tile.infected > 0 && tile.infected < tile.population then
      if tile.infected / tile.population * 100 > disease.tile_to_tile_spread then
        tiles.(row - 1).(column - 1) <- startTileInfection (tiles.(row - 1).(column - 1));
    tiles.(row - 1).(column) <- startTileInfection (tiles.(row - 1).(column));
    tiles.(row - 1).(column + 1) <- startTileInfection (tiles.(row - 1).(column + 1));
    tiles.(row).(column - 1) <- startTileInfection (tiles.(row).(column - 1));
    tiles.(row).(column + 1) <- startTileInfection (tiles.(row).(column + 1));
    tiles.(row + 1).(column) <- startTileInfection (tiles.(row + 1).(column));
    tiles.(row + 1).(column + 1) <- startTileInfection (tiles.(row + 1).(column + 1));
    tiles.(row + 1).(column - 1) <- startTileInfection (tiles.(row + 1).(column - 1));	
  | _ -> ()

let infectMap (map : Tile.t array array) (xy: int * int) = 
  startTileInfection map.(fst xy).(snd xy)
