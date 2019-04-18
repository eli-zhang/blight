open OUnit2
open Objects
open Infection
open Controller

let disease1 = Disease.{
    inner_tile_spread=100;
    tile_to_tile_spread=100;
    civ_to_civ_spread=100;
    spread_probability=100}

let civ1 = Civilization.{
    infected=ref 0;
    population=10;
    neighbors=[]
  }

let tile1 = Tile.{
    tile_type=Civ civ1;
    infected=0;
    population=2
  }

let civ_map1 = Civilization.{
    infected=ref 0;
    population=4;
    neighbors=[]
  }

let tile_map1 = Tile.{
    tile_type=Civ civ_map1;
    infected=0;
    population=2
  }

let civ_map3 = Civilization.{
    infected=ref 0;
    population=3;
    neighbors=[]
  }

let tile_map3 = Tile.{
    tile_type=Civ civ_map3;
    infected=0;
    population=1
  }

let tile2 = start_tile_infection tile1
let tile3 = infect_tile tile2 disease1
let map1 = Array.make 2 tile_map1
let map2 = Array.make 2 tile_map1
let infect_map2 = map2.(0) <- start_tile_infection map2.(0)
let more_infect_map2 = map2.(0) <- infect_tile map2.(0) disease1
let map3 = Array.make_matrix 3 1 tile_map3
let infect_map3 = map3.(0).(0) <- start_tile_infection map3.(0).(0)
let more_infect_map3 = map3.(0).(0) <- infect_tile map3.(0).(0) disease1
let spread_map3 = check_neighbors map3 0 0 disease1


let check_infection (name: string) (t: Tile.t) (bool: bool) : test = 
  name >:: (fun _ -> assert_equal bool ((t.infected) > 0))

let assert_infection (name: string) (t: Tile.t) (n: int) : test =
  name >:: (fun _ -> assert_bool ":(" (t.infected = n))

let check_civ_infection (name: string) (t: Civilization.t) (bool: bool) : test =
  name >:: (fun _ -> assert_equal bool (!(t.infected) > 0))

let assert_civ_infection (name: string) (t: Civilization.t) (n: int) : test =
  name >:: (fun _ -> assert_bool ":((" (!(t.infected) = n))

let infection_tests = [
  check_infection "uninfected tile" tile1 false;
  check_infection "infected tile" tile2 true;
  assert_infection "tile3" tile3 2;
  check_infection "map1.(0) false" map1.(0) false;
  check_infection "map1.(1) false" map1.(1) false;
  assert_infection "map2.(0) true" map2.(0) 2;
  check_infection "map2.(1) false" map2.(1) false;
  check_infection "map3.(0).(0) true" map3.(0).(0) true;
  check_infection "map3.(1).(0) true" map3.(1).(0) true;
  check_infection "map3.(2).(0) false" map3.(2).(0) false;
]

let test_suite = "test suites"  >::: List.flatten [
    infection_tests;
  ]

let _ = run_test_tt_main test_suite