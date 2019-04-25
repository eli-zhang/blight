open Objects

type upgrade = {
  inner_tile_boost: int;
  tile_to_tile_boost: int;
  water_boost: int;
  road_boost: int;
  spread_probability_boost: int;
  lethality_boost: int
}

let upgrade_list =
  ["Mosquito Transmission"; "Rat Transmission"; "Water Supply Infection";
   "Avian Transmission"; "Fever"; "Vomiting"; "Aggression"; "Internal Hemorrhaging";
   "Cannibalism"; "Uncontained Rabies"; "Total Collapse"]

let upgrade_disease (upgrade: upgrade) (disease: Disease.t) : Disease.t =
  Disease.{
    inner_tile_spread = min 100 (disease.inner_tile_spread + upgrade.inner_tile_boost);
    tile_to_tile_spread = min 100 (disease.tile_to_tile_spread + upgrade.tile_to_tile_boost);
    water_spread = min 100 (disease.water_spread + upgrade.water_boost);
    road_spread = min 100 (disease.road_spread + upgrade.road_boost);
    spread_probability = min 100 (disease.spread_probability + upgrade.spread_probability_boost);
    lethality = min 100 (disease.lethality + upgrade.lethality_boost)}

let mosquito_transmission : upgrade = {
  inner_tile_boost = 30;
  tile_to_tile_boost = 20;
  water_boost = 0;
  road_boost = 0;
  spread_probability_boost = 10;
  lethality_boost = 0;
}

let rat_transmission : upgrade = {
  inner_tile_boost = 20;
  tile_to_tile_boost = 30;
  water_boost = 0;
  road_boost = 0;
  spread_probability_boost = 10;
  lethality_boost = 0;
}

let water_supply_infection : upgrade = {
  inner_tile_boost = 0;
  tile_to_tile_boost = 0;
  water_boost = 60;
  road_boost = 0;
  spread_probability_boost = 0;
  lethality_boost = 0;
}

let avian_transmission : upgrade = {
  inner_tile_boost = 10;
  tile_to_tile_boost = 20;
  water_boost = 0;
  road_boost = 30;
  spread_probability_boost = 0;
  lethality_boost = 0;
}

let fever : upgrade = {
  inner_tile_boost = 0;
  tile_to_tile_boost = 0;
  water_boost = 0;
  road_boost = 0;
  spread_probability_boost = 0;
  lethality_boost = 5;
}

let vomiting : upgrade = {
  inner_tile_boost = 15;
  tile_to_tile_boost = 15;
  water_boost = 0;
  road_boost = 0;
  spread_probability_boost = 0;
  lethality_boost = 5;
}

let aggression : upgrade = {
  inner_tile_boost = 10;
  tile_to_tile_boost = 10;
  water_boost = 0;
  road_boost = 0;
  spread_probability_boost = 20;
  lethality_boost = 5;
}

let internal_hemorrhaging : upgrade = {
  inner_tile_boost = 0;
  tile_to_tile_boost = 0;
  water_boost = 0;
  road_boost = 0;
  spread_probability_boost = 0;
  lethality_boost = 20;
}

let cannibalism : upgrade = {
  inner_tile_boost = 20;
  tile_to_tile_boost = 20;
  water_boost = 0;
  road_boost = 0;
  spread_probability_boost = 0;
  lethality_boost = 20;
}

let uncontained_rabies : upgrade = {
  inner_tile_boost = 40;
  tile_to_tile_boost = 60;
  water_boost = 0;
  road_boost = 0;
  spread_probability_boost = 80;
  lethality_boost = 20;
}

let total_collapse : upgrade = {
  inner_tile_boost = 100;
  tile_to_tile_boost = 100;
  water_boost = 100;
  road_boost = 100;
  spread_probability_boost = 100;
  lethality_boost = 100;
}