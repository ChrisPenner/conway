{-# language TypeFamilies #-}
module Conway
  ( mkGrid
  , basicRule
  , step
  , render
  , beacon
  , glider
  , blinker
  , at
  , Grid
  , Rule
  ) where

import Data.Functor.Compose (Compose(..))
import Data.Vector (Vector, (!), generate)
import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)
import Control.Comonad (Comonad(..))

type Coord = (Int, Int)
type Grid = Store (Compose Vector Vector) Bool
type Rule = Grid -> Bool

instance Distributive Vector where
  distribute = distributeRep

instance Representable Vector where
  type Rep Vector = Int
  index v i = v ! (i `mod` gridSize)
  tabulate = generate gridSize

gridSize :: Int
gridSize = 20

neighbourCoords :: [Coord]
neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

addCoords :: Coord -> Coord -> Coord
addCoords (x, y) (x', y') = (x + x', y + y')

basicRule :: Rule
basicRule g = numNeighboursAlive == 3 || (alive && numNeighboursAlive == 2)
  where
    alive = extract g
    neighbours = experiment (at neighbourCoords) g
    numNeighboursAlive = length (filter id neighbours)

step :: Rule -> Grid -> Grid
step = extend

render :: Grid -> String
render (StoreT (Identity (Compose g)) _) = foldMap ((++ "\n") . foldMap (bool "." "#")) g

mkGrid :: [Coord] -> Grid
mkGrid xs = store (`elem` xs) (0, 0)

at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (addCoords origin) coords

glider, blinker, beacon :: [Coord]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]
