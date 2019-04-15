

type tile = | Land | Water | Air | Bridge | Civ

let map = Array.make_matrix 100 100 Land;;


let rec placeRiverUp map (x,y) =
  map.(x).(y) <- Water;
  map.(x+1).(y) <- Water;
  if(x<0 || y<0 || x>97 || y>98) then () else placeRiverUp map (x+1,y+1)

let rec placeRiverDown map (x,y)=
  map.(x).(y) <- Water;
  map.(x).(y-1)<- Water;
  if(x<1 || y<2 ||x>99||y>99) then () else placeRiverDown map (x-1,y-1)


let rec getCivilizations map n acc =
  let randx = Random.int 99 in
  let randy = Random.int 99 in 
  let size = Random.int 100 in
  match n with
  | 0 -> acc
  | _ -> getCivilizations map (n-1) ( ((randx,randy), size):: acc)


let rec closestNonCiv map (x,y) = 
  let dir = Random.int 4 in
  if (dir =1 && x-1 <> -1 && map.(x).(y) <> Civ) then (x-1,y)
  else if (dir = 2 &&x+1<>100 && map.(x).(y)<> Civ) then (x+1,y)
  else if (dir = 3 && y-1 <> -1 && map.(x).(y) <> Civ) then (x,y-1)
  else if (dir = 4 && y+1 <> 100 && map.(x).(y) <> Civ) then (x,y+1)
  else closestNonCiv map (x,y)



let rec placeCivilizations map civs = 
  match civs with
  | [] -> ();
  | h::t -> 
    let rec placeCiv ((x,y),size) =
      map.(x).(y) <- Civ;
      if (size > 0) then
        let coord = closestNonCiv map (x,y) in placeCiv (coord, size-1)
      else  ();
      placeCiv h;
      placeCivilizations map t




let generateWaterCiv map = 
  let randx = Random.int 99 in
  let randy = Random.int 99 in
  let nofciv = Random.int 20 in
  placeRiverUp map (randx,randy);
  placeRiverDown map (randx,randy);
  let civilizations = getCivilizations map nofciv [] in
  placeCivilizations map civilizations 







