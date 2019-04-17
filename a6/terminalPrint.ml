open Objects

let printMap map = 
  let rec printMap_helper = function
    | Tile.Land -> print_string "\027[40m  "; 
    | Tile.Water -> print_string "\027[46m  "; 
    | Tile.Road -> print_string "\027[0m  ";
    | Tile.Civ t -> print_string "\027[47m  "; in

  let rec printMap_helper2 map count =
    if count = (Array.length map) then  ( print_string "\027[0m\n \027[31mDone." )  
    else Array.iter printMap_helper (Array.get map count);  
    print_string "\027[0m\n"; printMap_helper2 map (count+1) 
  in printMap_helper2  map 0 