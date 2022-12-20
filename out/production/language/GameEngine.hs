module GameEngine where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, randomR)
import Data.List

import Store
import Data.Map (Map)
import Data.Set
import qualified Data.Map as Map

-- check if two lists overlap
hasOverlapping :: [Coord] -> [Coord] -> Bool
hasOverlapping a b =  not (Data.List.null (a `intersect` b))

-- inc/dec with boud
decBound, incBound :: (Ord a, Num a) => a -> a -> a
decBound x b = max b (x - 1)
incBound x b = min b (x + 1)

-- move the player in the current direction
movePlayerInDirection :: GameState -> GameState
movePlayerInDirection (Playing player lst1 lst2 dir rnd step keyMap colors) = Playing (moveCoordInDirection player dir) lst1 lst2 dir rnd step keyMap colors

-- move a coord in a direction
moveCoordInDirection :: Coord -> Direction -> Coord
moveCoordInDirection (x, y) N = (x, y + 1)
moveCoordInDirection (x, y) S = (x, y - 1)
moveCoordInDirection (x, y) E = (x + 1, y)
moveCoordInDirection (x, y) W = (x - 1, y)

-- set the direction
setDirection :: Direction -> GameState -> GameState
setDirection dir (Playing player lst1 lst2 _ rnd step keyMap colors) = Playing player lst1 lst2 dir rnd step keyMap colors

-- place random good without overlap
placeRandomGood :: GameState -> GameState
placeRandomGood (Playing player lst1 lst2 dir rnd step keyMap colors) = Playing player lst1' lst2 dir rnd' step keyMap colors
  where (rnd', lst1') = placeRandomGood' rnd lst1 lst2 player
placeRandomGood' :: StdGen -> [Coord] -> [Coord] -> Coord -> (StdGen, [Coord])
placeRandomGood' rnd lst1 = checkOverlap rnd'' (lst1++[(fromIntegral x', fromIntegral y')])
  where (x', rnd') = getRandomX rnd
        (y', rnd'') = getRandomY rnd'

-- check if two lists overlap
checkOverlap :: StdGen -> [Coord] -> [Coord] -> Coord -> (StdGen, [Coord])
checkOverlap rnd lst1 lst2 player
  | player `elem` lst1 || hasOverlapping lst1 lst2 = placeRandomGood' rnd (init lst1) lst2 player
  | otherwise                                      = (rnd, lst1)
  
-- clear the GOOD list
clearGood :: GameState -> GameState
clearGood (Playing player _ lst2 dir rnd step keyMap colors) = Playing player [] lst2 dir rnd step keyMap colors

-- clear the BAD list
clearBad :: GameState -> GameState
clearBad (Playing player lst1 _ dir rnd step keyMap colors) = Playing player lst1 [] dir rnd step keyMap colors

-- check if coordinate is in game
isOnMap :: Coord -> Bool
isOnMap (x, y) = not ((x < 0) || (x > xsize) || (y < 0) || (y > ysize))

-- replace item at index
replaceAtIndex n item ls = a ++ (item:b)
  where (a, _ : b) = Data.List.splitAt n ls

-- remove duplicates
removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = toList . fromList

-- get Random X or Y
getRandomX :: StdGen -> (Int, StdGen)
getRandomX = randomR (0, xsize)
getRandomY :: StdGen -> (Int, StdGen)
getRandomY = randomR (0, ysize)

-- bound player to size of map
boundPlayer :: Coord -> Coord
boundPlayer (x, y)
  | x < 0 = boundPlayer (0, y)
  | y < 0 = boundPlayer (x, 0)
  | x > xsize = boundPlayer (xsize, y)
  | y > ysize = boundPlayer (x, ysize)
  | otherwise = (x, y)
