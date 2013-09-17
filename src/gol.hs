module Gol where
--import Conway (World, Location)

type Location = (Int, Int)

data Cell = Alive | Dead

type World = [(Location, Cell)]

instance Show Cell where
  show Alive = "Alive Cell"
  show Dead = "Dead Cell"


-- Returns the max coordinates of a world
max_coordinate :: World -> Location
max_coordinate xs  = aux xs (0, 0)
  where aux [] loc = loc
        aux (x:xs') loc = if (fst x) > loc
                          then aux xs' (fst x)
                          else aux xs' loc

-- Some test conditions
-- let a= [((0,0), Alive), ((1,0), Dead), ((1,1), Dead)] :: World
-- max_coordinate a

cell_state :: World -> Location -> Cell
cell_state world loc = case world of [] -> Dead
                                     (x:xs') -> if (fst x) == loc
                                                then snd x
                                                else cell_state xs' loc


-- Returns the number of neighbours for a cell
-- neighbours :: World -> Location -> Int
-- neighbours world loc = 
