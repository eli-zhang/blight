open Objects

let random_check threshold =
  (Random.int 100) < threshold

(** [start_tile_infection tile] starts an infection in tile [tile] 
    (1 person infected) if it is not infected and does nothing 
    to the tile if it is. It then returns the tile. *)
let start_tile_infection (tile :Tile.tile_types): Tile.tile_types =
  match tile with
  | Civ civ -> if !(civ.infected) = 0 
    then (civ.infected := 1; Civ{civ with infected = ref 1})
    else if !(civ.infected )= 0 then 
      ((civ.infected := !(civ.infected) + 1);
       Civ{civ with infected = ref 1})
    else tile
  | _ -> tile

(** [infect_tile tile disease] spreads the infection in tile [tile] if it 
    contains any infected people (increases infected number). If [tile] is not 
    infected, it does nothing. *)
let infect_tile (tile : Tile.tile_types) (disease : Disease.t) : Tile.tile_types =
  match tile with
  | Civ civ -> if !(civ.infected) = 0 then tile else 
      let new_infected = int_of_float 
          (floor (((float_of_int disease.inner_tile_spread)
                   *. float_of_int !(civ.living)) /. 100.0 +. 0.5))
                         + !(civ.infected) in 
      let died = if random_check disease.lethality
        then (let max_dead = int_of_float 
                  (floor (((float_of_int !(civ.living)) *.  
                           (float_of_int disease.lethality) /. 100.0) +. 0.5)) in
              let died_count = 
                if max_dead > !(civ.living) 
                then !(civ.living) else max_dead in
              civ.living := (!(civ.living) - died_count);
              civ.dead := (!(civ.dead) + died_count);
              died_count)
        else 0 in

      if new_infected > civ.population 
      then (civ.infected := !(civ.infected) + civ.population - !(civ.infected);
            Tile.Civ{civ with infected = ref (civ.population);
                              living = ref (!(civ.living) - died);
                              dead = ref(!(civ.dead) + died)})
      else (civ.infected := !(civ.infected) + new_infected - !(civ.infected);
            Tile.Civ{civ with infected = ref new_infected;
                              living = ref(!(civ.living)- died);
                              dead =  ref (!(civ.dead) + died)})
  | _ -> tile

(** [check_neighbors tiles row column disease] checks if a tile at a given [row]
    and [column] in the map [tiles] is over the disease [disease]'s infection 
    threshold for spreading to neighboring tiles. If it is, the function 
    randomly spreads to neighboring tiles based 
    on the disease's random spread probability. *)
let check_neighbors (tiles: Tile.tile_types array array) row column (disease: Disease.t) =
  let tile = tiles.(row).(column) in
  let rows = Array.length tiles - 1 in
  let cols = Array.length tiles.(row) - 1 in

  match tile with
  | Civ total_infected -> if !(total_infected.infected) > 0 then
      if 100 * !(total_infected.infected) / total_infected.population >= disease.tile_to_tile_spread then
        if !(total_infected.living) > 0 then
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