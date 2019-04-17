

type tile = | Land | Water | Air | Bridge | Civ

let length = 40
let width= 40

let map = Array.make_matrix length width Land;;


let rec placeRiverUp map (x,y) =
  map.(x).(y) <- Water;
  map.(x+1).(y) <- Water;
  if(x<0 || y<0 || x>(length-3) || y>(width-2))  then () else placeRiverUp map (x+1,y+1)

let rec placeRiverDown map (x,y)=
  map.(x).(y) <- Water;
  map.(x).(y-1)<- Water;
  if(x<1 || y<2 ||x>(length-1) ||y>(width-1)) then () else placeRiverDown map (x-1,y-1)


let rec getCivilizations map n acc =
  let randx = Random.int (length-1) in
  let randy = Random.int (width-1) in 
  if randx= 0 || randx = (length-1) || randy = 0 || randy = (width-1) || map.(randx).(randy) = Civ || map.(randx+1).(randy) = Civ||
     map.(randx-1).(randy) = Civ|| map.(randx).(randy+1) = Civ || map.(randx).(randy-1) = Civ then getCivilizations map n acc else
    let size = Random.int 10 in
    match n with
    | 0 -> acc
    | _ -> getCivilizations map (n-1) ( ((randx,randy), size):: acc)


let rec closestNonCiv map (x,y) = 
  let dir = Random.int 4 in
  if (dir =1 && x-1 <> -1 && map.(x-1).(y) <> Civ) then (x-1,y)
  else if (dir = 2 &&x+1<>length && map.(x+1).(y)<> Civ) then (x+1,y)
  else if (dir = 3 && y-1 <> -1 && map.(x).(y-1) <> Civ) then (x,y-1)
  else if (dir = 4 && y+1 <> width && map.(x).(y+1) <> Civ) then (x,y+1)
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
    in 
    placeCiv h;
    placeCivilizations map t

let generateWaterCiv map = 
  let randx = Random.int (length-1) in
  let randy = Random.int (width-1) in
  let nofciv = (Random.int 30) +10 in
  placeRiverUp map (randx,randy);
  placeRiverDown map (randx,randy);

  let civilizations = getCivilizations map 5 [] in
  placeCivilizations map civilizations