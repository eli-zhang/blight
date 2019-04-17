(* open Objects  *)

let printMap map = 
  let rec printMap_helper (tile: Tile.t) =
    match tile.tile_type with
    | Land -> print_string "\027[40m  "; 
    | Water -> print_string "\027[46m  "; 
    | Road -> print_string "\027[0m  ";
    | Civ civ -> let infected = !(civ.infected) in let population = civ.population in
      if infected > 0 then if infected = population then (print_string "\027[31m\027[47m≡≡") else
        if infected > population/2 then (print_string "\027[31m\027[47m::")
        else (print_string "\027[31m\027[47m··")
      else (print_string "\027[47m  ";) in 

  let rec printMap_helper2 map count =
    if count = (Array.length map) then  ( print_string "\027[0m\n \027[31mDone." )  
    else (Array.iter printMap_helper (Array.get map count);  
          print_string "\027[0m\n"; printMap_helper2 map (count+1) )
  in (print_string "\027[0;0H"; printMap_helper2  map 0 )