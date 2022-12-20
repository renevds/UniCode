module Store where
  
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (StdGen)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type X = Int
type Y = Int
type Coord = (X, Y)
--size of map
xsize, ysize, xres, yres :: Int
xsize = 20
ysize = 20
xres = 1000
yres = 1000
-- default player start position
playerStartPosition :: Coord
playerStartPosition = (10, 0)

-- all expressions that return an int
data IntExp = Lit Int
            | IntExp :+: IntExp
            | IntExp :*: IntExp
            | IntExp :-: IntExp
            | IntExp :/: IntExp
            | IntExp :%: IntExp

            | Not IntExp
            | IntExp :||: IntExp
            | IntExp :&&: IntExp
            | IntExp :==: IntExp
            | IntExp :>: IntExp
            | IntExp :<: IntExp
            | IntExp :>=: IntExp
            | IntExp :<=: IntExp
            | IntExp :!=: IntExp
            
            | ReadVar String
            | ExpCall String [IntExp]
            
            | GameExpressionCall GameExpression

-- expression that returns a coordinate
type CoordExp = (IntExp, IntExp)

-- expressions that change the store
data Command = Seq Command Command
             | Assign String IntExp
             | Print IntExp
             | If IntExp Command Command
             | While IntExp Command
             | EmptyCommand
             | Function String [String] Command
             | Call String [IntExp]
             | Scope Command
             | GameCommandCall GameCommand

-- game-specific commands
data GameCommand = StartGame IntExp
                 | CreateGame String
                 | StateTransformer (GameState -> GameState)
                 | SetDirection String
                 | Bind String String
                 | SetScore IntExp
                 | SetColor IntExp IntExp IntExp IntExp String
                 | GoodBad String ([Coord] -> [Coord])
                 | AddTo String CoordExp
                 | AddRandomBadToRow IntExp
                 | ForEach String String
                 
-- game-specific intexp
data GameExpression = AmountOfGood
                    | StateExpression (GameState -> Int)
                    | GameLit Int

type Score = Int

-- directions
data Direction = N | S | W | E

-- map for all binded keys
type KeyMap = Map SpecialKey Command

-- first list is good
-- second list is bad
data GameState = Playing Coord [Coord] [Coord] Direction StdGen String KeyMap [Color]
               | Score Int

-- default actor colors
defaultColors :: [Color]
defaultColors = [blue, green, red]

-- variable names for buildins
returnVarName, stateVarName :: String
stateVarName = "_state"
returnVarName = "_return"

-- values that can be a variable
data Value = IntVal Int
           | FunVal [String] Command
           | GameState GameState
 
instance Show Value where
    show (IntVal i)  = show i
    show (FunVal _ _)  = "function()"

-- to store the variables in
type Store = Map String Value

-- lookup variable in store
storeLookup :: String -> Store -> Value
storeLookup x m = case Map.lookup x m of
  Just v -> v
  Nothing -> error ("Variable or Function '"++x++"' does not exist in this scope.")

-- lookup binded key in keyMap
keyMapLookup :: SpecialKey -> KeyMap -> Command
keyMapLookup x m = case Map.lookup x m of
  Just v -> v
  Nothing -> error "Keybind does not exist."

-- to add binden key to keyMap
addKeyMap :: String -> Command -> Store -> IO Store
addKeyMap "LEFT" c store = addKeyMap' KeyLeft c store
addKeyMap "RIGHT" c store = addKeyMap' KeyRight c store
addKeyMap "UP" c store = addKeyMap' KeyUp c store
addKeyMap "DOWN" c store = addKeyMap' KeyDown c store
addKeyMap "SPACE" c store = addKeyMap' KeySpace c store

addKeyMap' :: SpecialKey -> Command -> Store -> IO Store
addKeyMap' key c store = do
                         let (Playing player lst1 lst2 dir rnd step keyMap colors) = getStateFromStore store;
                         let keyMap' = Map.insert key c keyMap;
                         return(updateStateInStore store (Playing player lst1 lst2 dir rnd step keyMap' colors))

-- shortcut to get game state from store
getStateFromStore :: Store -> GameState
getStateFromStore store = state
  where GameState state = storeLookup stateVarName store

-- shortcut to update game state from store
updateStateInStore :: Store -> GameState -> Store
updateStateInStore store state = Map.insert stateVarName (GameState state) store;

-- keep variable in one store if the key was in another store
addIfExistsInOtherStore :: String -> Store -> Store -> IO Store
addIfExistsInOtherStore var from to = do
                                        if Map.member var from
                                        then return(Map.insert var (storeLookup var from) to)
                                        else return to

-- keep variables in one store if their key was in another store
addAllIfExistsInOtherStore :: Store -> Store -> Store
addAllIfExistsInOtherStore from check = Map.fromList fromList
  where checkList = map fst (Map.toList check)
        fromList  = [(key, val)| (key, val)<-Map.toList from, key `elem` checkList]