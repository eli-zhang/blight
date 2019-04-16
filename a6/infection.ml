open Tile
open Civilization
open Disease

module StartInfection (T:Tile) (D:Disease) : Tile = struct
  type tile_type = T.tile_type
  let population = T.population
  let infected = match T.tile_type with
    | Civ -> if T.infected = 0 then 1 else 0
    | _ -> 0
end

module InfectTile (T:Tile) (D:Disease) : Tile = struct
  type tile_type = T.tile_type
  let population = T.population
  let infected = if T.infected = 0 then 0 else
      let new_infected = (D.inner_tile_spread * T.population / 100) + T.infected in 
      if new_infected > T.population then population else new_infected
end 
