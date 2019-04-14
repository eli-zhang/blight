

type tile = | Land | Water | Air | Bridge | Civ

let map = Array.make_matrix 100 100 Land;;


let rec placeRiverUp map (x,y) =
  map.(x).(y) <- 10;
  map.(x+1).(y) <- 10;
  if(x<0 || y<0 || x>97 || y>98) then () else placeRiverUp map (x+1,y+1)

let rec placeRiverDown map (x,y)=
  map.(x).(y) <- 10;
  map.(x).(y-1)<- 10;
  if(x<1 || y<2 ||x>99||y>99) then () else placeRiverDown map (x-1,y-1)



let generateWater map = 
  let randx = Random.int 99 in
  let randy = Random.int 99 in
  placeRiverUp map (randx,randy);
  placeRiverDown map (randx,randy)






