
module type Civilization = sig 
  type civ 
  val infection : int ref
  val population : int
  val neighbors : (civ * int * int) list
end

open Disease

module ModifyCiv : functor (C: Civilization) -> (D: Disease) -> Civilization

end