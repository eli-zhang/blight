open Objects

let map = Array.make_matrix 40 50 Tile.Land

let civs = Array.make 10 (0,0)



(** [getCivilizations map n acc] returns a list with length n that represents 
    civilizations *)
let rec getCivilizations map n acc s civs=
  let length = (Array.length map) in
  let width = (Array.length (Array.get map 0)) in
  let randx = Random.int (length) in
  let randy = Random.int (width) in 
  if randx= 0 || randx = (length-1) || randy = 0 || randy = (width-1) 
     || (match map.(randx).(randy) with | Tile.Civ _ -> true  | _ -> false)
  then getCivilizations map n acc s civs else
    let size = Random.int s+5 in
    match n with
    | 0 -> acc
    | _ -> civs.(n-1) <- (randx,randy); getCivilizations map (n-1) (((randx,randy), size) :: acc ) s civs

(** [placeCivilizations map civs] indexes the matrix map with civilization 
    tiles using the information in civs  *)
let rec placeCivilizations map civs = 
  let length = (Array.length map) in
  let width = (Array.length (Array.get map 0)) in
  let rec closestNonCiv map (x,y) = 
    let dir = Random.int 4 in
    if (  dir = 0 && x-1 > -1) then (x-1,y)
    else if ( dir = 1 &&x+1 < (length) ) then (x+1,y)
    else if ( dir=2 &&y-1 > -1 ) then (x,y-1)
    else if ( dir = 3&& y+1 < width ) then (x,y+1)
    else closestNonCiv map (x,y) in
  match civs with
  | [] -> ();
  | h::t -> 
    let rec placeCiv ((x,y),size) =
      map.(x).(y) <- Tile.Civ{infected = ref 0; living = ref 50; dead = ref 0;population = 50; neighbors = [] };
      if (size > 0) then
        let coord = closestNonCiv map (x,y) in placeCiv (coord, size-1)
      else  ();
    in 
    placeCiv h;
    placeCivilizations map t

(** [getBridges map civs staticciv] indexes the matrix map with bridges that
    connect each civ in civs to the other civ in civs, staticciv is the same list
    as civs, *)
let rec getBridges map civs staticciv=
  let length = (Array.length map) in
  let width = (Array.length (Array.get map 0)) in
  match civs with
  | [] -> ();
  | h::t -> 
    let rec connectCivs (coords,size) civs' = 
      match civs' with 
      | [] -> ();
      | (c,s)::t ->
        (let rec makeBridge (x1,y1) (x2,y2) =
           if(x1<x2 && x1<length-1 ) then  
             (if (map.(x1+1).(y1)= Tile.Civ{infected = ref 0;living = ref 50; dead = ref 0;population = 50;neighbors = []}) then makeBridge ((x1+1),y1) (x2,y2)
              else map.(x1+1).(y1) <- Tile.Road; makeBridge (x1+1,y1) (x2,y2) )
           else if (x1>x2 && x1>0) then 
             (if (map.(x1-1).(y1) = Tile.Civ{infected = ref 0;living = ref 50; dead = ref 0;population = 50;neighbors = []}) then makeBridge ((x1-1),y1) (x2,y2)
              else map.(x1-1).(y1) <- Tile.Road; makeBridge (x1-1,y1) (x2,y2))
           else if (y1<y2 && y1<width-1) then 
             (if (map.(x1).(y1+1) = Tile.Civ{infected = ref 0;living = ref 50; dead = ref 0;population = 50;neighbors = []}) then makeBridge (x1,(y1+1)) (x2,y2)
              else map.(x1).(y1+1) <- Tile.Road; makeBridge (x1,(y1+1)) (x2,y2))
           else if (y1>y2 && y1>0) then 
             (if (map.(x1).(y1-1) = Tile.Civ{infected = ref 0;living = ref 50; dead = ref 0;population = 50;neighbors = []}) then makeBridge (x1,(y1-1)) (x2,y2)
              else map.(x1).(y1-1) <- Tile.Road; makeBridge (x1,(y1-1)) (x2,y2))
           else () in
         makeBridge c coords; connectCivs (coords,size) t) in 
    connectCivs h staticciv; getBridges map t staticciv



let rec placeRivers map n = 
  let length = (Array.length map) in
  let width = (Array.length (Array.get map 0)) in
  let rec placeRiverUp map (x,y) =
    map.(x).(y) <-Tile.Water;
    map.(x+1).(y) <- Water;
    if(x>(-2) && y>(-2) && x<(length-2) && y<(width-1))  
    then placeRiverUp map (x+1,y+1) else () in

  let rec placeRiverDown map (x,y)=
    map.(x).(y) <- Tile.Water;
    map.(x).(y-1)<-Tile.Water;
    if(x>(0) && y>(1) && x<(length) &&y<(width)) 
    then placeRiverDown map (x-1,y-1) else () in
  if n>0 then  
    let randx = Random.int (length-1) in
    let randy = Random.int (width-1) in
    placeRiverUp map (randx,randy);
    placeRiverDown map (randx,randy);
    placeRivers map (n-1)
  else ()

(**[generateMap map] simply indexes the matrix map with the water, bridges, and civ tiles
   in the correct spots *)
let generateMap map civs size= 
  let nofcivs = Array.length civs in


  let nofrivs = nofcivs/3 in
  placeRivers map nofrivs;
  let civilizations = getCivilizations map nofcivs [] size civs in
  placeCivilizations map civilizations;
  getBridges map civilizations civilizations




(** [printMap map] prints the matrix map *)
let printMap map = 

  let rec printMap_helper  = function
    | Tile.Land -> print_string "\027[40m  "; 
    | Tile.Water -> print_string "\027[46m  "; 
    | Tile.Road -> print_string "\027[0m  ";
    | Tile.Civ _ -> print_string "\027[47m  "; in

  let rec printMap_helper2 map count =
    if (count = Array.length map) then 
      (print_string "\027[0m\n \027[31mDone." )
    else ( (Array.iter printMap_helper (Array.get map count;));
           print_string "\027[0m\n"; printMap_helper2 map (count+1) )

  in
  printMap_helper2  map 0 




