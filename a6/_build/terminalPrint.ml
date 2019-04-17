open Objects

let printMap map = 
  let printMap_helper (tile: Tile.t) = 
    match tile.tile_type with
    | Land -> print_string "\027[40m  "; 
    | Water -> print_string "\027[46m  "; 
    | Road -> print_string "\027[0m  ";
    | Civ t -> print_string "\027[47m  "; in

  let rec printMap_helper2 map count =
    if count = (Array.length map) then  ( print_string "\027[0m\n \027[31mDone." )  
    else Array.iter printMap_helper (Array.get map count);  
    print_string "\027[0m\n"; printMap_helper2 map (count+1) 
  in printMap_helper2  map 0 