open Objects
let civ1 = Civilization.{infected = ref 10; population= 10; neighbors= [] }
let civ2 = Civilization.{civ1 with infected = ref 1}
let map = Array.make_matrix 5 5 Tile.(Civ civ1)
