type disease = {
  inner_tile_spread : int;
  tile_to_tile_spread : int;
  civ_to_civ_spread : int;
}

type civilization = {
  infected : int ref;
  population : int;
  neighbors : (civilization ref * tile ref) list;
} 
and tile = Civ of civilization | Land | Water | Road

(* let rec infect_civilizations (acc: civilization list) 
    (civilizations: civilization list) (disease: disease) =
   match civilizations with
   | [] -> acc
   | (civ::t) -> 
    let infected = civ.infected in
    let population = civ.population in
    let neighbors = 
      if (!infected / population * 100) > disease.civ_to_civ_spread 
      then let rec infect_neighbors neighbors =
             match neighbors with
             | [] -> ();
             | (civ, tile)::t -> if !(civ.infected) = 0 
               then civ.infected := 1;
               tile.infected := 1; 
               infect_neighbors t in
        infect_neighbors C.neighbors;
        C.neighbors


          infect_civilizations (InfectCivilizations (civ) (disease))::acc t disease

   end *)