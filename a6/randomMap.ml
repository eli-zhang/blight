
open Objects


let map = Array.make_matrix 40 50 Tile.Land

let civs = Array.make 10 (0,0)



(** [getCivilizations map n acc] returns a list with length n that represents 
    civilizations *)
let rec getCivilizations (map: Tile.t array array) n acc s civs=
  Random.self_init ();
  let length = (Array.length map) in
  let width = (Array.length (Array.get map 0)) in
  let randx = Random.int (length) in
  let randy = Random.int (width) in 
  if randx= 0 || randx = (length-1) || randy = 0 || randy = (width-1) 
     || (match map.(randx).(randy).tile_type with | Civ _ -> true  | _ -> false)
  then getCivilizations map n acc s civs else
    let size = s in
    match n with
    | 0 -> ((randx,randy),size):: acc
    | _ -> civs.(n-1) <- (randx,randy); getCivilizations map (n-1) (((randx,randy), 8) :: acc ) s civs



(** [placeCivilizations map civs] indexes the matrix map with civilization 
    tiles using the information in civs  *)
let rec placeCivilizations (map: Tile.t array array) civs = 
  let length = (Array.length map) in
  let width = (Array.length (Array.get map 0)) in
  let rec closestNonCiv (map: Tile.t array array) (x,y) = 
    let dir = Random.int 4 in
    if (  dir = 0 && x-1 > -1) then if (match (map.(x-1).(y)).tile_type with | Civ _ -> false | _ -> true) then (x-1,y) else closestNonCiv map (x,y)
    else if ( dir = 1 &&x+1 < (length) ) then if (match (map.(x+1).(y)).tile_type with | Civ _ -> false | _ -> true) then (x+1,y) else closestNonCiv map (x,y)
    else if ( dir=2 &&y-1 > -1 )  then if (match (map.(x).(y-1)).tile_type with | Civ _ -> false | _ -> true) then (x,y-1) else closestNonCiv map (x,y)
    else if ( dir = 3&& y+1 < width ) then if (match (map.(x).(y+1)).tile_type with | Civ _ -> false | _ -> true) then (x,y+1) else closestNonCiv map (x,y)
    else if (x-1 > -1 && x+1< length && y+1<width && y-1 > -1 ) then if
      ((match (map.(x-1).(y)).tile_type with | Civ _ -> true | _ -> false) &&
       (match (map.(x+1).(y)).tile_type with | Civ _ -> true | _ -> false) &&
       (match (map.(x).(y-1)).tile_type with | Civ _ -> true | _ -> false) &&
       (match (map.(x).(y+1)).tile_type with | Civ _ -> true | _ -> false)
      ) then closestNonCiv map (x+1,y) else closestNonCiv map (x+1,y)

    else closestNonCiv map (x,y) in
  match civs with
  | [] -> ();
  | (c,s)::t -> 
    let civilization = Civilization.{infected =ref 0; living =ref (50*(s+1)); dead= ref 0; population=50*(s+1);neighbors=[] } in
    let rec placeCiv ((x,y),size) =
      match (map.(x).(y)).tile_type with 
      | Civ _ -> map.(x).(y) <- Tile.{tile_type = Civ civilization; infected =0; living = 50; dead=0;population = 50};
      | _ -> map.(x).(y) <- Tile.{tile_type = (Civ civilization);
                                  infected = 0; living= 50; dead=0; population =50 };
        if (size > 0) then
          let coord = closestNonCiv map (x,y) in placeCiv (coord, size-1)
        else  ();
    in 
    placeCiv (c,s);
    placeCivilizations map t

(** [getBridges map civs staticciv] indexes the matrix map with bridges that
    connect each civ in civs to the other civ in civs, staticciv is the same list
    as civs, *)
let rec getBridges (map: Tile.t array array) civs staticciv=
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

             match (map.(x1+1).(y1)).tile_type with
             | Civ _ -> makeBridge ((x1+1),y1) (x2,y2)
             | _ -> map.(x1+1).(y1) <- Tile.{tile_type = Road 0; infected = 0; living =0; dead=0; population =0}; makeBridge (x1+1,y1) (x2,y2)

           else if (x1>x2 && x1>0) then 
             match (map.(x1-1).(y1)).tile_type with
             | Civ _ -> makeBridge ((x1-1),y1) (x2,y2)
             | _ -> map.(x1-1).(y1) <- Tile.{tile_type = Road 0; infected = 0; living =0; dead=0; population =0}; makeBridge (x1-1,y1) (x2,y2)

           else if (y1<y2 && y1<width-1) then 
             match (map.(x1).(y1+1)).tile_type with
             | Civ _ -> makeBridge ((x1),y1+1) (x2,y2)
             | _ -> map.(x1).(y1+1) <- Tile.{tile_type = Road 0; infected = 0; living =0; dead=0; population =0}; makeBridge (x1,y1+1) (x2,y2)

           else if (y1>y2 && y1>0) then 
             match (map.(x1).(y1-1)).tile_type with
             | Civ _ -> makeBridge ((x1),y1-1) (x2,y2)
             | _ -> map.(x1).(y1-1) <- Tile.{tile_type = Road 0; infected = 0; living =0; dead=0; population =0}; makeBridge (x1,y1-1) (x2,y2)

           else () in
         makeBridge c coords; connectCivs (coords,size) t) in 
    connectCivs h staticciv; getBridges map t staticciv



let rec placeRivers (map: Tile.t array array) n = 
  let length = (Array.length map) in
  let width = (Array.length (Array.get map 0)) in
  let rec placeRiverUp (map: Tile.t array array) (x,y) =
    map.(x).(y) <-Tile.{tile_type= Water 0; infected= 0; living=0;dead=0;population=0};
    map.(x+1).(y) <- Tile.{tile_type= Water 0; infected= 0; living=0;dead=0;population=0};
    if(x>(-2) && y>(-2) && x<(length-2) && y<(width-1))  
    then placeRiverUp map (x+1,y+1) else () in

  let rec placeRiverDown (map: Tile.t array array) (x,y)=
    map.(x).(y) <- Tile.{tile_type= Water 0; infected= 0; living=0;dead=0;population=0};
    map.(x).(y-1)<-Tile.{tile_type= Water 0; infected= 0; living=0;dead=0;population=0};
    if(x>(0) && y>(1) && x<(length) &&y<(width)) 
    then placeRiverDown map (x-1,y-1) else () in
  if n>0 then  
    let randx = Random.int (length-1) +1 in
    let randy = Random.int (width-1) + 1 in
    placeRiverUp map (randx,randy);
    placeRiverDown map (randx,randy);
    placeRivers map (n-1)
  else ()

(**[generateMap map civs] simply indexes the matrix map with the water, bridges, and civ tiles
   in the correct spots and sets the coordinate of one civ tile of each civilization to each
   element in array civs *)
let generateMap (map: Tile.t array array) civs= 
  Random.self_init ();
  let nofcivs = Array.length civs in


  let nofrivs = nofcivs/3 in
  placeRivers map nofrivs;
  let civilizations = getCivilizations map nofcivs [] 8 civs in
  placeCivilizations map civilizations;
  getBridges map civilizations civilizations




(** [printMap map] prints the matrix map *)
let printMap (map: Tile.t array array) = 

  let rec printMap_helper (tile:Tile.t) = 
    match tile.tile_type with
    | Land -> print_string "\027[40m  "; 
    | Water _ -> print_string "\027[46m  "; 
    | Road _ -> print_string "\027[0m  ";
    | Civ _ -> print_string "\027[47m  "; in

  let rec printMap_helper2 (map: Tile.t array array) count =
    if (count = Array.length map) then 
      (print_string "\027[0m\n \027[31mDone." )
    else ( (Array.iter printMap_helper (Array.get map count;));
           print_string "\027[0m\n"; printMap_helper2 map (count+1) )

  in
  printMap_helper2  map 0 









