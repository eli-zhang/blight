open Objects
let civ = Civilization.{infected = ref 0; population= 10; neighbors= [] }
let map = Array.make_matrix 5 5 Tile.(Civ civ)