open Objects

let startTileInfection (tile : tile): tile =
  match tile with
  | (Civ civ) -> if civ.infected = 0 then Civ {civ with infected = 1} else Civ civ
  | _ -> tile

let infectTile (tile : tile) (disease : tile) : tile =
  match tile with
  | Civ civ -> if civ.infected = 0 then Civ civ else 
      let new_infected = (disease.inner_tile_spread * civ.population / 100) + civ.infected
      in if new_infected > civ.population then Civ {civ with infected = civ.population}
      else Civ {civ with infected = new_infected}
  | _ -> tile

let infectMap (map : tile array array) (xy: int * int) = 
  startTileInfection map.(fst xy).(snd xy)
