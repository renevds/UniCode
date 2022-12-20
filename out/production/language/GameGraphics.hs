module GameGraphics where

import Graphics.Gloss
import GameEngine
import Graphics.Gloss.Interface.Environment
import Store
import Data.List

-- score
scoreText, continueText :: String
scoreText = "You scored: "
continueText = "Press any key to quit."

-- colors
inactiveColor :: Color
inactiveColor = makeColorI 50 50 50 256

-- constants
dblock  = 12 -- de zijde van 1 blokje (inclusief marge rondom)
dwidth  = 10 -- de zijde van 1 blokje (exclusief marge, inclusief randje)
dinner  = 7 -- de zijde van 1 blokje (enkel het zwarte stuk middenin)
fscale  = 3.5 -- algemene schaal van de hele tekening

fullxsize, fullysize :: Int
fullxsize = xsize*dblock
fullysize = ysize*dblock

--- <-----------------------> dblock
---     <---------------> dwidth
---         <-------> dinner
---     +---------------+
---     |               |
---     |   MMMMMMMMM   |
---     |   MMMMMMMMM   |
---     |   MMMMMMMMM   |
---     |               |
---     +---------------+


pixel :: Picture
pixel = Pictures [rectangleWire dwidth dwidth, rectangleSolid dinner dinner]

-- an empty pixel centered about the origin
inactivePixel :: Picture
inactivePixel = Color inactiveColor pixel

-- a line of empty pixels
inactiveLine :: Int -> Picture
inactiveLine x = Pictures [Translate (fromIntegral x) (fromIntegral y) inactivePixel | y <- [0, dblock .. fullysize]]

-- a full map of empty pixels
emptyBoard :: Picture
emptyBoard = Pictures [inactiveLine x | x <- [0, dblock .. fullxsize]]

-- center and scale to fit screen
centerAndScale :: Picture -> Picture
centerAndScale pic = Scale fscale fscale (Translate (fromIntegral ((-fullxsize) `div` 2)) (fromIntegral ((-fullysize) `div` 2)) pic)

-- a filled pixel at coordinate
drawCoord :: Coord -> Picture
drawCoord (x, y) = Translate (fromIntegral (x*dblock)) (fromIntegral (y*dblock)) pixel

-- draw colored pixel at coordinate
drawCoordWithColor ::  Color -> Coord -> Picture
drawCoordWithColor color coord = Color color (drawCoord coord)

-- draw list of colored pixels
drawCoordListWithColor :: Color -> [Coord] -> Picture
drawCoordListWithColor color coords = Pictures (map (drawCoordWithColor color) coords)

-- some text constants
textScale :: Float
textScale = 0.1

leftAlign :: Int
leftAlign = 0

lineHeight :: Int
lineHeight = 20

-- centered text
centeredText :: String -> Picture
centeredText str = Translate (fromIntegral leftAlign) (fromIntegral (fullysize `div` 2)) (Scale textScale textScale (Text str))

-- centered lines of text
centeredLines :: [String] -> Picture
centeredLines strs = Pictures [Translate 0.0 (fromIntegral (-(lineHeight*i))) (centeredText x) | (i,x)  <- zip [1..] strs]

-- render the game
renderGame :: Store -> IO Picture
renderGame store = return (centerAndScale (renderGame' (getStateFromStore store)));
renderGame' :: GameState -> Picture
renderGame' (Playing player lst1 lst2 _ _ _ _ colors) =  Pictures [emptyBoard, drawCoordListWithColor (colors!!2) lst2, drawCoordListWithColor (colors!!1) lst1, drawCoordWithColor (head colors) player]
renderGame' (Score s) = Pictures [Color green (centeredText (scoreText ++ show s)), Translate 0.0 (fromIntegral (-(fullysize `div` 2))) (Color white (centeredText continueText))]

-- set color of actor
setColor :: Int -> Int -> Int -> Int -> String -> GameState -> GameState
setColor r g b a str (Playing player lst1 lst2 dir rnd step keyMap colors) = Playing player lst1 lst2 dir rnd step keyMap (replaceAtIndex index (makeColorI r g b a) colors)
  where index = head (elemIndices str ["PLAYER", "GOOD", "BAD"])