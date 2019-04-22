open Objects
let civ1 = Civilization.{infected = ref 0; living = ref 10; dead = ref 0;
                         population= 10; neighbors= [] }
let civ2 = Civilization.{civ1 with infected = ref 1}
let civ3 = Civilization.{civ2 with infected = ref 6}
let civ4 = Civilization.{civ3 with infected = ref 10}
let map = Array.make_matrix 5 5 Tile.(Civ civ1)
