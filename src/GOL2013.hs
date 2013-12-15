module GOL where

-- Another attempt (at Code Retreat)

import Data.Maybe (fromJust)
import Data.List (find)

type Location = (Int, Int)

type Length = Int
type Width = Int

data State = Alive | Dead deriving (Eq, Show)

data Cell = Cell {
  location :: Location,
  cellState :: State
                 } deriving (Eq, Show)

createGrid :: Length -> Width -> [Cell]
createGrid l w = [Cell (a, b) Dead | a <- [0..l-1], b <- [0..w-1]]

-- Finds all the neighbors of the cell
neighbors :: Location -> [Cell] -> [Cell]
neighbors (a,b) cells = filter (\c -> location c `elem` fLocs) cells
  where neighLocs = [(a, b) | a <- [(a-1)..(a+1)], b <- [(b-1)..(b+1)]]
        fLocs = removeLocation (a,b) $ filter (\(x,y) -> x >= 0 && y >= 0) neighLocs

-- Removes paricular element from Location
removeLocation :: Location -> [Location] -> [Location]
removeLocation loc locs = filter (\l -> loc/=l) locs

-- Finds all the Alive neighbors of the Cell
aliveNeighbors :: Location -> [Cell] -> [Cell]
aliveNeighbors loc cells = filter (\cell -> cellState cell == Alive) neighs
  where neighs = neighbors loc cells

-- Finds all the Dead neighbors of the cell
deadNeighbors :: Location -> [Cell] -> [Cell]
deadNeighbors loc cells = filter (\cell -> cellState cell == Dead) neighs
  where neighs = neighbors loc cells        

-- Find the number of alive neighbors of the Cell
numAliveNeighbors :: Location -> [Cell] -> Int
numAliveNeighbors loc cells = length $ aliveNeighbors loc cells

-- Find the number of dead neighbors of the Cell
numDeadNeighbors :: Location -> [Cell] -> Int
numDeadNeighbors loc cells = length $ deadNeighbors loc cells

isAlive :: Location -> [Cell] -> Bool
isAlive loc cells = cellState cell == Alive
  where cell = fromJust $ find (\c -> location c == loc) cells

conwayRule :: [Cell] -> Location -> Cell
conwayRule cells loc = case isAlive loc cells of
  True -> if numAliveNeigh < 2 || numAliveNeigh > 3
          then Cell loc Dead
          else Cell loc Alive
  False -> case numAliveNeigh == 3 of
    True -> Cell loc Alive
    False -> Cell loc Dead
  where numAliveNeigh = numAliveNeighbors loc cells
        numDeadNeigh = numDeadNeighbors loc cells

nextGeneration :: [Cell] -> [Cell]
nextGeneration cells = map (conwayRule cells) allLocs
  where allLocs = map location cells
