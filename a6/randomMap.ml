

type tile = | Land | Water | Air | Bridge | Civ

let length = 40
let width= 50

let map = Array.make_matrix length width Land;;




(** [getCivilizations map n acc] returns a list with length n that represents civilizations *)
let rec getCivilizations map n acc =
  let randx = Random.int (length-1) in
  let randy = Random.int (width-1) in 
  if randx= 0 || randx = (length-1) || randy = 0 || randy = (width-1) || map.(randx).(randy) = Civ || map.(randx+1).(randy) = Civ||
     map.(randx-1).(randy) = Civ|| map.(randx).(randy+1) = Civ || map.(randx).(randy-1) = Civ then getCivilizations map n acc else
    let size = Random.int 5+5 in
    match n with
    | 0 -> acc
    | _ -> getCivilizations map (n-1) ( ((randx,randy), size):: acc)




(** [placeCivilizations map civs] indexes the matrix map with civilization tiles using the
    information in civs  *)
let rec placeCivilizations map civs = 
  let rec closestNonCiv map (x,y) = 
    let dir = Random.int 4 in
    if (dir =1 && x-1 <> -1 && map.(x-1).(y) <> Civ) then (x-1,y)
    else if (dir = 2 &&x+1<>length && map.(x+1).(y)<> Civ) then (x+1,y)
    else if (dir = 3 && y-1 <> -1 && map.(x).(y-1) <> Civ) then (x,y-1)
    else if (dir = 4 && y+1 <> width && map.(x).(y+1) <> Civ) then (x,y+1)
    else closestNonCiv map (x,y) in
  match civs with
  | [] -> ();
  | h::t -> 
    let rec placeCiv ((x,y),size) =
      map.(x).(y) <- Civ;
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
  match civs with
  | [] -> ();
  | h::t -> 
    let rec connectCivs (coords,size) civs' = 
      match civs' with 
      | [] -> ();
      | (c,s)::t ->
        (let rec makeBridge (x1,y1) (x2,y2) =
           if(x1<x2 && x1<length-1 ) then  
             (if (map.(x1+1).(y1)=Civ) then makeBridge ((x1+1),y1) (x2,y2)
              else map.(x1+1).(y1) <- Bridge; makeBridge (x1+1,y1) (x2,y2) )
           else if (x1>x2 && x1>0) then 
             (if (map.(x1-1).(y1) = Civ) then makeBridge ((x1-1),y1) (x2,y2)
              else map.(x1-1).(y1) <- Bridge; makeBridge (x1-1,y1) (x2,y2))
           else if (y1<y2 && y1<length-1) then 
             (if (map.(x1).(y1+1) = Civ) then makeBridge (x1,(y1+1)) (x2,y2)
              else map.(x1).(y1+1) <- Bridge; makeBridge (x1,(y1+1)) (x2,y2))
           else if (y1>y2 && y2>0) then 
             (if (map.(x1).(y1-1) = Civ) then makeBridge (x1,(y1-1)) (x2,y2)
              else map.(x1).(y1-1) <- Bridge; makeBridge (x1,(y1-1)) (x2,y2))
           else () in
         makeBridge c coords; connectCivs (coords,size) t) in 



    connectCivs h staticciv; getBridges map t staticciv






(**[generateWaterCiv map] simply indexes the matrix map with the water, bridges, and civ tiles
   in the correct spots *)
let generateWaterCiv map = 
  let rec placeRiverUp map (x,y) =
    map.(x).(y) <- Water;
    map.(x+1).(y) <- Water;
    if(x<0 || y<0 || x>(length-3) || y>(width-2))  then () else placeRiverUp map (x+1,y+1) in

  let rec placeRiverDown map (x,y)=
    map.(x).(y) <- Water;
    map.(x).(y-1)<- Water;
    if(x<1 || y<2 ||x>(length-1) ||y>(width-1)) then () else placeRiverDown map (x-1,y-1) in
  let randx = Random.int (length-1) in
  let randy = Random.int (width-1) in
  let nofciv = (Random.int 30) +10 in
  placeRiverUp map (randx,randy);
  placeRiverDown map (randx,randy);

  let civilizations = getCivilizations map 5 [] in
  placeCivilizations map civilizations;
  getBridges map civilizations civilizations




(** [printMap map] prints the matrix map *)
let printMap map = 
  let rec printMap_helper  = function
    | Land -> print_string "\027[40m  "; 
    | Water -> print_string "\027[46m  "; 
    | Bridge -> print_string "\027[0m  ";
    | Civ -> print_string "\027[47m  "; in

  let rec printMap_helper2 map count =
    if (count = Array.length map) then  ( print_string "\027[0m\n \027[31mDone." )  else ( (Array.iter printMap_helper (Array.get map count)));  print_string "\027[0m\n"; printMap_helper2 map (count+1) 

  in
  printMap_helper2  map 0 




