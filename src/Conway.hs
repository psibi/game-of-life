module Conway where

import Data.List (nub)

type Location = (Int, Int)

data Cell = Alive | Dead deriving (Eq)

type World = [(Location, Cell)]

instance Show Cell where
  show Alive = "Alive Cell"
  show Dead = "Dead Cell"
