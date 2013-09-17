module Gol where
--import Conway (World, Location)

import Data.List (nub)

type Location = (Int, Int)

data Cell = Alive | Dead deriving (Eq)

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

-- Returns whether the cell is Alive or Dead
cell_state :: World -> Location -> Cell
cell_state world loc = case world of [] -> Dead
                                     (x:xs') -> if (fst x) == loc
                                                then snd x
                                                else cell_state xs' loc


-- Returns all the neighbours for a cell
neighbours :: Location -> [Location]
neighbours loc = case loc of (a,b) -> filter (\x -> x /= loc) all_neighbours
                                      where all_neighbours = [(x,y) | x <- [a-1..a+1], y <- [b-1..b+1],  x >= 0, y >= 0]

-- Returns the number of alive neighbours for a cell
alive_neighbours :: World -> Location -> Int
alive_neighbours world loc = foldl (\acc x -> if x==Alive then acc+1 else acc) 0 neighbour_states
    where neighbour_states = map (\x -> cell_state world x) (neighbours loc)

--time_step :: World -> World
--time_step world = 

--let world_data = [((0,0), Alive), ((0,1), Dead), ((0,2), Alive), ((0,3), Dead),((1,0), Alive), ((1,1), Dead), ((1,2), Alive), ((1,3), Dead),((2,0), Alive), ((2,1), Alive), ((2,2), Dead), ((2,3), Alive),((3,0), Alive), ((3,1), Dead), ((3,2), Alive), ((3,3), Alive)] :: World
