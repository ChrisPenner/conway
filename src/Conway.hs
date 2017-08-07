{-# language GeneralizedNewtypeDeriving #-}
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

import Data.Functor.Compose
import qualified Data.Vector as V
import Data.Distributive
import Data.Functor.Rep
import Control.Arrow ((***))
import Control.Comonad.Representable.Store
import Control.Comonad
import Control.Monad (guard)

type Rule = Grid Bool -> Bool
type Coord = (Int, Int)
type Grid a = Store (Compose VBounded VBounded) a

newtype VBounded a = VBounded (V.Vector a)
  deriving (Eq, Show, Functor)

instance Distributive VBounded where
  distribute = distributeRep

instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i = v V.! i
  tabulate desc = VBounded $ V.generate gridSize desc

gridSize :: Int
gridSize = 20

wrap :: Int -> Int
wrap = (`mod` gridSize)

neighbourCoords :: [(Int, Int)]
neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

boardCoords :: [[Coord]]
boardCoords = [[(x, y) | y <- [0 .. gridSize - 1]] | x <- [0 .. gridSize - 1]]

basicRule :: Rule
basicRule g =
  (alive && numNeighbours `elem` [2, 3]) || (not alive && numNeighbours == 3)
  where
    alive = extract g
    numNeighbours = length (filter id neighbours)
    val True = 1
    val False = 0
    addCoords (x, y) = (+x) *** (+y)
    neighbours = experiment (\s -> fmap ((wrap *** wrap) . addCoords s) neighbourCoords) g

step :: Rule -> Grid Bool -> Grid Bool
step = extend

render :: Grid Bool -> String
render s = foldMap ((++ "\n") . foldMap toS) (fmap (`peek` s) <$> boardCoords)
  where
    toS True = "#"
    toS False = "."

mkGrid :: [Coord] -> Grid Bool
mkGrid xs = store lookup (0, 0)
  where
    lookup crd = crd `elem` xs

at :: [Coord] -> Coord -> [Coord]
at xs (x, y) = fmap ((+x) *** (+y)) xs

glider, blinker, beacon :: [Coord]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]
