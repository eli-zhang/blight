open Objects

let length = 24
let width = 40

let map = Array.make_matrix length width Tile.Land;;

(** [getCivilizations map n acc] returns a list with length [n] 
    that represents civilizations *)
let rec getCivilizations map n acc =
  let randx = Random.int (length-1) in
  let randy = Random.int (width-1) in 
  if randx = 0 || randx = (length - 1) || randy = 0 || randy = (width - 1) 
     || match map.(randx).(randy) with | Tile.Civ _ -> true | _ ->
     match map.(randx+1).(randy) with | Tile.Civ _ -> true | _ ->
     match map.(randx-1).(randy) with | Tile.Civ _ -> true | _ ->
     match map.(randx).(randy+1) with | Tile.Civ _ -> true | _ ->
     match map.(randx).(randy-1) with | Tile.Civ _ -> true | _ -> false
  then getCivilizations map n acc else
    let size = Random.int 5+5 in
    match n with
    | 0 -> acc
    | _ -> getCivilizations map (n-1) ( ((randx,randy), size):: acc)

(** [placeCivilizations map civs] indexes the matrix [map] with civilization 
    tiles using the information in [civs]  *)
let rec placeCivilizations map civs = 
  let rec closestNonCiv map (x,y) = 
    let dir = Random.int 4 in

    if (dir = 1 && x-1 <> -1) then
      match map.(x-1).(y) with
      | Tile.Civ _ ->
        begin
          if (dir = 2 && x+1 <> length) then 
            match map.(x+1).(y) with
            | Tile.Civ _ ->
              begin
                if (dir = 3 && y-1 <> -1) then
                  match map.(x).(y-1) with
                  | Tile.Civ _ ->
                    begin
                      if (dir = 4 && y+1 <> width) then
                        match map.(x).(y+1) with
                        | Tile.Civ _ -> closestNonCiv map (x,y)
                        | _ -> (x, y+1)
                      else (x, y+1)
                    end
                  | _ -> (x, y-1)
                else (x, y-1)
              end
            | _ -> (x+1, y)
          else (x+1, y)
        end
      | _ -> (x-1, y)
    else (x-1, y) in

  match civs with
  | [] -> ();
  | h::t -> 
    let rec placeCiv ((x,y),size) =
      let example_civ = 
        Civilization.{infected = ref 0; population = 80000; neighbors= []} in
      map.(x).(y) <- Tile.Civ example_civ;
      if (size > 0) then
        let coord = closestNonCiv map (x,y) in 
        placeCiv (coord, size-1)
      else (); in 
    placeCiv h;
    placeCivilizations map t

(** [getRoads map civs staticciv] indexes the matrix [map] with roads that
    connect each civ in [civs] to the other civ in civs.
    staticciv is the same list as civs. *)
let rec getRoads map civs staticciv =
  match civs with
  | [] -> ();
  | h::t -> 
    let rec connectCivs (coords,size) civs' = 
      match civs' with 
      | [] -> ();
      | (c,s)::t ->
        (let rec makeRoad (x1,y1) (x2,y2) =
           if(x1<x2 && x1<length-1) then
             match map.(x1+1).(y1) with
             | Tile.Civ _ -> makeRoad ((x1+1),y1) (x2,y2)
             | _ -> map.(x1+1).(y1) <- Tile.Road; makeRoad (x1+1,y1) (x2,y2)
           else if (x1>x2 && x1>0) then 
             match map.(x1-1).(y1) with 
             | Tile.Civ _ -> makeRoad ((x1-1),y1) (x2,y2)
             | _ -> map.(x1-1).(y1) <- Tile.Road; makeRoad (x1-1,y1) (x2,y2)
           else if (y1<y2 && y1<width-1) then 
             match map.(x1).(y1+1) with
             | Tile.Civ _ -> makeRoad (x1,(y1+1)) (x2,y2)
             | _ -> map.(x1).(y1+1) <- Tile.Road; makeRoad (x1,(y1+1)) (x2,y2)
           else if (y1>y2 && y1>0) then 
             match map.(x1).(y1-1) with
             | Tile.Civ _ -> makeRoad (x1,(y1-1)) (x2,y2)
             | _ -> map.(x1).(y1-1) <- Tile.Road; makeRoad (x1,(y1-1)) (x2,y2)
           else () in
         makeRoad c coords; connectCivs (coords,size) t) in 
    connectCivs h staticciv; getRoads map t staticciv

(**[generateWaterCiv map] simply indexes the matrix map with the water, 
   roads, and civ tiles in the correct spots *)
let generateWaterCiv map = 
  let rec placeRiverUp map (x,y) =
    map.(x).(y) <- Tile.Water;
    map.(x+1).(y) <- Tile.Water;
    if (x < 0 || y < 0 || x > (length - 3) || y > (width - 2)) 
    then () else placeRiverUp map (x + 1, y + 1) in

  let rec placeRiverDown map (x,y)=
    map.(x).(y) <- Tile.Water;
    map.(x).(y-1) <- Tile.Water;
    if (x < 1 || y < 2 || x > (length - 1) || y > (width - 1)) 
    then () else placeRiverDown map (x - 1, y - 1) in
  let randx = Random.int (length - 1) in
  let randy = Random.int (width - 1) in
  (* let nofciv = (Random.int 30) +10 in *)
  placeRiverUp map (randx,randy);
  placeRiverDown map (randx,randy);

  let civilizations = getCivilizations map 5 [] in
  placeCivilizations map civilizations;
  getRoads map civilizations civilizations

(** [printMap map] prints the matrix map *)
let printMap map = 
  let rec printMap_helper  = function
    | Tile.Land -> print_string "\027[40m  "; 
    | Tile.Water -> print_string "\027[46m  "; 
    | Tile.Road -> print_string "\027[0m  ";
    | Tile.Civ _ -> print_string "\027[47m  "; in

  let rec printMap_helper2 map count =
    if (count = Array.length map) then (print_string "\027[0m\n \027[31mDone.")  
    else ((Array.iter printMap_helper (Array.get map count)));  
    print_string "\027[0m\n"; printMap_helper2 map (count+1) 

  in
  printMap_helper2  map 0 