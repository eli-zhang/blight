type disease = {
  inner_tile_spread : int;
  tile_to_tile_spread : int;
  civ_to_civ_spread : int;
}

type civilization = {
  total_infected : int ref;
  total_population : int;
  neighbors : (civilization * tile) list;
} 
and tile_type = Land | Water | Road | Civ of civilization
and tile = {
  tile_type: tile_type;
  population: int;
  infected: int ref;
}
(* let rec infect_civilizations (acc: civilization list) 
    (civilizations: civilization list) (disease: disease) =
   match civilizations with
   | [] -> acc
   | (civ::t) -> 
    let infected = civ.total_infected in
    let population = civ.total_population in
    let neighbors = 
      if (!infected / population * 100) > disease.civ_to_civ_spread 
      then let rec infect_neighbors neighbors =
             match neighbors with
             | [] -> ();
             | (civ, tile)::t -> if !(civ.total_infected) = 0 
               then civ.total_infected := 1;
               tile.infected := 1; 
               infect_neighbors t in
        infect_neighbors civ.neighbors in
        neighbors *)