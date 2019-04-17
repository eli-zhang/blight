open Objects 

let printMap map time = 
  let rec printMap_helper (tile: Tile.t) =
    let colors = true in
    if colors then 
      (* Prints the map in colored tiles *)
      match tile.tile_type with
      | Land -> print_string "\027[40m  "; 
      | Water -> print_string "\027[46m  "; 
      | Road -> print_string "\027[0m  ";
      | Civ civ -> 
        let infected = tile.infected in 
        let population = tile.population in
        let ratio = 100 * infected / population in
        if ratio > 0 then 
          if ratio = 100 then (print_string "\027[41m  ") else
          if ratio = 66 then (print_string "\027[31m\027[47m::")
          else (print_string "\027[31m\027[47m··")
        else (print_string "\027[47m  ";)

    else 
      (* Prints information about each tile in the map *)
      match tile.tile_type with
      | Land -> print_string "\027[40m L"; 
      | Water -> print_string "\027[46m W"; 
      | Road -> print_string "\027[0m R";
      | Civ civ -> let infected = tile.infected in let population = tile.population in
        print_string ("I: " ^ string_of_int infected 
                      ^ " P: " ^ string_of_int population ^ " "); in

  let rec printMap_helper2 map count time =
    if count = (Array.length map) then 
      (print_endline ("\027[0m\n\027[31mElapsed Time: " ^ (string_of_int time)))
    else (Array.iter printMap_helper (Array.get map count);  
          print_string "\027[0m\n"; printMap_helper2 map (count+1) time)
  in (print_string "\027[0;0H"; printMap_helper2  map 0 time)