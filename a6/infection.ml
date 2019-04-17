open Objects

let startTileInfection (tile : Tile.t): Tile.t =
  match tile with
  | Civ civ -> if !(civ.infected) = 0 
    then (civ.infected := 1; tile)
    else tile
  | _ -> tile

let infectTile (tile : Tile.t) (disease : Disease.t) : Tile.t =
  match tile with
  | Civ civ -> if !(civ.infected) = 0 then Civ civ else 
      let new_infected = (disease.inner_tile_spread * civ.population / 100) 
                         + !(civ.infected) in
      if new_infected > civ.population 
      then (civ.infected := civ.population; tile)
      else (civ.infected := new_infected; tile)
  | _ -> tile

let infectMap (map : Tile.t array array) (xy: int * int) = 
  startTileInfection map.(fst xy).(snd xy)
