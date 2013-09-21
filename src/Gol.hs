module Gol where
import Conway

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
num_alive_neighbours :: World -> Location -> Int
num_alive_neighbours world loc = foldl (\acc x -> if x==Alive then acc+1 else acc) 0 neighbour_states
    where neighbour_states = map (\x -> cell_state world x) (neighbours loc)

-- Returns the location of all alive neighbours
alive_neighbours :: World -> Location -> [Location]
alive_neighbours world loc = filter (\x -> cell_state world x == Alive) (neighbours loc)

-- Returns the location of all dead neighbours
dead_neighbours :: World -> Location -> [Location]
dead_neighbours world loc = filter (\x -> cell_state world x == Dead) (neighbours loc)

-- Returns all the Alive cell present in the world.
alive_cells :: World -> [Location]
alive_cells world = map (\x -> fst x) alive
  where alive = filter (\x -> cell_state world (fst x) == Alive) world

-- Apply Conway GOL Rule for a Cell
game_of_life :: World -> Location -> (Location,Cell)
game_of_life world loc = case (cell_state world loc) of Dead -> if num_alive_neighbours world loc == 3
                                                                then (loc, Alive)
                                                                else (loc, Dead)
                                                        Alive -> num_alive_neighbours_cells world loc
  where num_alive_neighbours_cells world loc
          | num_alive_neighbours world loc > 3 = (loc, Dead)
          | num_alive_neighbours world loc < 2 = (loc, Dead)
          | num_alive_neighbours world loc == 2 || num_alive_neighbours world loc == 3 = (loc, Alive)
        
-- One time step for the game of life
time_step :: World -> World
time_step world = map (\x -> game_of_life world (fst x)) world

--let world_data = [((0,0), Alive), ((0,1), Dead), ((0,2), Alive), ((0,3), Dead),((1,0), Alive), ((1,1), Dead), ((1,2), Alive), ((1,3), Dead),((2,0), Alive), ((2,1), Alive), ((2,2), Dead), ((2,3), Alive),((3,0), Alive), ((3,1), Dead), ((3,2), Alive), ((3,3), Alive)] :: World

--let another_world_data = [((0,0), Dead), ((0,1), Alive), ((0,2), Dead), ((1,0), Dead), ((1,1), Alive), ((1,2), Dead), ((2,0), Dead), ((2,1), Alive), ((2,2), Dead)] :: World
