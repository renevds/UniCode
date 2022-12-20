module Interpreter where

import Control.Monad
import Parser
import Store
import Data.Map (Map)
import qualified Data.Map as Map
import System.Exit
import Data.List

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GameGraphics
import GameEngine
import System.Random (StdGen, getStdGen)


-----------------
---Expressions---
-----------------

-- evaluate an IntExp
evalIntExp :: IntExp -> Store -> IO Value
evalIntExp e store = do
                       i <- evalIntExp' e store
                       return(IntVal i)

evalIntExp' :: IntExp -> Store -> IO Int
evalIntExp' (Lit n) store     = return n
evalIntExp' (e :+: f) store   = do
                                  e <- evalIntExp' e store
                                  f <- evalIntExp' f store
                                  return(e + f)
evalIntExp' (e :*: f) store   = do
                                  e <- evalIntExp' e store
                                  f <- evalIntExp' f store
                                  return(e * f)
evalIntExp' (e :-: f) store   = do
                                  e <- evalIntExp' e store
                                  f <- evalIntExp' f store
                                  return(e - f)
evalIntExp' (e :/: f) store   = do 
                                  e <- evalIntExp' e store
                                  f <- evalIntExp' f store
                                  return(e `div` f)
evalIntExp' (e :%: f) store   = do
                                  e <- evalIntExp' e store
                                  f <- evalIntExp' f store
                                  return(e `mod` f)

evalIntExp' (Not e) store     = do
                                  e <- evalIntExp' e store
                                  return(fromEnum(e==0))
evalIntExp' (e :||: f) store  = do 
                                  e <- evalIntExp' e store
                                  f <- evalIntExp' f store
                                  return(fromEnum ((e /= 0) || (f /= 0)))
evalIntExp' (e :&&: f) store  = do
                                  e <- evalIntExp' e store
                                  f <- evalIntExp' f store
                                  return(fromEnum ((e /= 0) && (f /= 0)))
evalIntExp' (e :==: f) store  = do
                                  e <- evalIntExp' e store
                                  f <- evalIntExp' f store
                                  return(fromEnum(e==f))
evalIntExp' (e :>: f) store   = do
                                  e <- evalIntExp' e store
                                  f <- evalIntExp' f store
                                  return(fromEnum(e>f)) 
evalIntExp' (e :<: f) store   = do 
                                  e <- evalIntExp' e store
                                  f <- evalIntExp' f store
                                  return(fromEnum(e<f))
evalIntExp' (e :>=: f) store  = do
                                  e <- evalIntExp' e store
                                  f <- evalIntExp' f store
                                  return(fromEnum(e>=f))
evalIntExp' (e :<=: f) store  = do
                                  e <- evalIntExp' e store
                                  f <- evalIntExp' f store
                                  return(fromEnum(e<=f))
evalIntExp' (e :!=: f) store  = do
                                  e <- evalIntExp' e store
                                  f <- evalIntExp' f store
                                  return(fromEnum(e/=f))
                                                     
evalIntExp' (ReadVar var) store = do
                                    let (IntVal i) = storeLookup var store;
                                    return i;
evalIntExp' (ExpCall n a) store = do
                                    s <- evalCommand (Call n a) store;
                                    let (IntVal i) = storeLookup returnVarName s;
                                    return i;

evalIntExp' (GameExpressionCall gameExp) store = evalGameExpression gameExp store;

-- parse a two argument operator such as +
parseOperator :: String -> Parser(IntExp, IntExp)
parseOperator t = do
                  token '('
                  d <- parseIntExp
                  cMatch t;
                  e <- parseIntExp
                  cToken ')'
                  return (d, e)
                  

-- parse an IntExp
parseIntExp :: Parser IntExp
parseIntExp = parseIgn `mplus` parseLit `mplus` parseAdd `mplus` parseMul `mplus` parseSub `mplus` parseDiv `mplus` parseMod
              `mplus` parseEQ `mplus` parseST `mplus` parseGT `mplus` parseGTE `mplus` parseSTE `mplus` parseNE `mplus` parseRead `mplus` parseCall
              `mplus` parseAOfG `mplus` parseOR `mplus` parseAND `mplus` parseNOT
  where
  parseIgn  = do
                 ignoredTokenMatcher
                 parseIntExp
  parseLit  = do
                Lit <$> parseInt
  parseAdd  = do
                (d, e) <- parseOperator "+";
                return (d :+: e)
  parseMul  = do
                (d, e) <- parseOperator "*";
                return (d :*: e)
  parseSub  = do
                (d, e) <- parseOperator "-";
                return (d :-: e) 
  parseDiv  = do
                (d, e) <- parseOperator "/";
                return(d :/: e) 
  parseMod  = do
                (d, e) <- parseOperator "%";
                return(d :%: e)

  parseNOT  = do
                token '('
                cToken '¬'
                e <- parseIntExp
                cToken ')'
                return(Not e)

  parseAND  = do
                (d, e) <- parseOperator "∧";
                return(d :&&: e)
  parseOR   = do
                (d, e) <- parseOperator "∨";   
                return(d :||: e)
  parseEQ   = do
                (d, e) <- parseOperator "⟺"; 
                return(d :==: e)
  parseGT   = do
                (d, e) <- parseOperator ">"; 
                return(d :>: e)
  parseST   = do
                (d, e) <- parseOperator "<"; 
                return(d :<: e)
  parseGTE  = do
                (d, e) <- parseOperator "≥"; 
                return(d :>=: e)
  parseSTE  = do
                (d, e) <- parseOperator "≤"; 
                 return(d :<=: e)
  parseNE   = do
                (d, e) <- parseOperator "≠"; 
                return(d :!=: e)
  parseRead = do
                ReadVar <$> parseVar
  parseCall = do
                name <- parseVar
                cToken '('
                args <- parseIntExps
                cToken ')'
                return (ExpCall name args)
  parseAOfG = do
                token '🎮';
                GameExpressionCall <$> parseGameExpression
                   
--parse a List of IntExp (for arguments of functions)
parseIntExps :: Parser [IntExp]
parseIntExps = exps `mplus` exp `mplus` noExp
  where
  exps = do
           e1 <- parseIntExp
           cToken ','
           ignoredTokenMatcher
           e2 <- parseIntExps
           return (e1:e2)
  exp  = do
           e1 <- parseIntExp;
           return [e1]
  noExp = do
            return []

-----------------
----Commands-----
-----------------

-- evaluate a command
evalCommand :: Command -> Store -> IO Store
evalCommand (Assign var val) store       = do
                                             e <- evalIntExp val store;
                                             return (Map.insert var e store)
evalCommand (Function name args c) store = do
                                             return (Map.insert name (FunVal args c) store)
evalCommand (Call name argVals) store    = do
                                              s <- addExpressionsToList args argVals store
                                              evalCommand c s
  where (FunVal args c) = storeLookup name store
evalCommand (Print val) store            = do
                                             e <- evalIntExp val store
                                             print e
                                             return store
evalCommand (Seq c1 c2) store            = do
                                             s <- evalCommand c1 store
                                             evalCommand c2 s   
evalCommand EmptyCommand store           = return store
evalCommand (If e c1 c2) store           = do
                                             (IntVal i) <- evalIntExp e store
                                             if i /=0
                                             then evalCommand c1 store
                                             else evalCommand c2 store
evalCommand (While e c) store            = do 
                                             (IntVal i) <- evalIntExp e store;
                                             if i /=0
                                             then 
                                               do
                                                 s <- evalCommand c store
                                                 evalCommand (While e c) s
                                             else return store
evalCommand (Scope c) store              = do
                                             s <- evalCommand c store;
                                             store' <- addIfExistsInOtherStore returnVarName s store
                                             store'' <- addIfExistsInOtherStore stateVarName s store'
                                             return (addAllIfExistsInOtherStore s store'')

evalCommand (GameCommandCall gc) store   = evalGameCommand gc store

-- for functions with one int argument
parseIntFunction :: String -> Parser IntExp
parseIntFunction t = do
                       match (t++"(")
                       val <- parseIntExp
                       cToken ')'
                       return val
                       
-- for functions with one var argument                 
parseVarFunction :: String -> Parser String
parseVarFunction t = do
                       match (t++"(")
                       val <- parseVar
                       cToken ')'
                       return val

-- parse a command
parseCommand :: Parser Command
parseCommand = parseIgnore `mplus` parseSeq `mplus` parseIf `mplus` parseIfElse `mplus` parseWhile `mplus` parseAssign
               `mplus` parseFunc `mplus` parseCall `mplus` parseReturn `mplus` parseIncr `mplus` parsePrint
               `mplus` parseEngine
  where
  parseIgnore = do
                  ignoredTokenMatcher
                  parseCommand
  parseSeq    = do
                  token '{'
                  c <- parseDelim
                  cToken '}'
                  return c
  parseIf     = do
                  match "?("
                  e <- parseIntExp
                  cToken ')'
                  c1 <- parseSeq
                  return(If e c1 EmptyCommand)
  parseIfElse = do
                  match "?("
                  e <- parseIntExp
                  cToken ')'
                  c1 <- parseSeq
                  cToken '¿'
                  If e c1 <$> parseSeq
  parseWhile  = do
                  match "🔁("
                  e <- parseIntExp
                  cToken ')'
                  While e <$> parseSeq
  parseAssign = do
                  var <- parseVar;
                  cToken '='
                  Assign var <$> parseIntExp
  parseFunc   = do 
                  token '⎔'
                  ignoredTokenMatcher
                  name <- parseVar
                  cToken '('
                  args <- parseArgs
                  cToken ')'
                  Function name args . Scope <$> parseSeq
  parseCall   = do
                  name <- parseVar
                  cToken '('
                  args <- parseIntExps
                  cToken ')'
                  return (Call name args)
  parseIncr   = do
                  var <- parseVar
                  cMatch "++"
                  return (Assign var (ReadVar var :+: Lit 1)) 
  parsePrint  = do
                  val <- parseIntFunction "📺"
                  return(Print val)
  parseNon    = do
                  ignoredTokenMatcher
                  return EmptyCommand 
  parseDelim  = do 
                  c1 <- parseCommand
                  cToken ';'
                  c2 <- parseDelim `mplus` parseNon
                  return(Seq c1 c2)
  parseReturn  = do
                   match "⮑("
                   val <- parseIntExp
                   cToken ')'
                   return(Assign returnVarName val)
  parseEngine  = do
                   match "🎮"
                   GameCommandCall <$> parseGameCommand

-- parse a list of argument names (for functions)
parseArgs :: Parser [String]
parseArgs = args `mplus` arg `mplus` noArg
  where
  args  = do
            a1 <- parseVar
            cToken ','
            ignoredTokenMatcher
            a2 <- parseArgs
            return (a1:a2)
  arg   = do
            a1 <- parseVar;
            return [a1]
  noArg = do
            return []

-- add a list of argument variables annd their value to the store
addExpressionsToList :: [String] -> [IntExp] -> Store -> IO Store
addExpressionsToList args argExps store = do
                                            argEvals <- mapM (`evalIntExp` store) argExps
                                            let argMap = Map.fromList (zip args argEvals)
                                            let un = Map.union argMap store
                                            return un

--------------------------
-----Engine Commands------
--------------------------
-- evaluate the gamestep function
gameStepEval :: Float -> Store -> IO Store
gameStepEval t store = do
                         let state = getStateFromStore store;
                         gameStepEval' state store;

gameStepEval' :: GameState -> Store -> IO Store
gameStepEval' (Score _) store = return store
gameStepEval' (Playing _ _ _ _ _ stepF _ _) store = do
                                                      let (FunVal args stepCommand) =  storeLookup stepF store
                                                      evalCommand stepCommand store

-- evaluate the binded functions
eventHandlerEval :: Event -> Store -> IO Store
eventHandlerEval (EventKey (SpecialKey key)  Down _ _)  store = do
                                                                  let state = getStateFromStore store
                                                                  eventHandlerEval' key state store
                                                                  
eventHandlerEval _ store = return store

eventHandlerEval' :: SpecialKey -> GameState -> Store -> IO Store
eventHandlerEval' key (Score _) _                                                 = exitSuccess
eventHandlerEval' key (Playing player lst1 lst2 dir rnd step keyMap colors) store = do
                                                                                      if Map.member key keyMap
                                                                                      then do
                                                                                        evalCommand (keyMapLookup key keyMap) store
                                                                                      else
                                                                                        return store

-- evaluate a game-specific command
evalGameCommand:: GameCommand -> Store -> IO Store
evalGameCommand (CreateGame stepF) store          = do
                                                      stdGen <- getStdGen
                                                      return(Map.insert stateVarName (GameState (Playing playerStartPosition [] [] N stdGen stepF Map.empty defaultColors)) store)
  
evalGameCommand (StartGame stpExp) store          = do
                                                      (IntVal steps) <- evalIntExp stpExp store;
                                                      playIO (InWindow "Game running on uniCODE" (xres, yres) (10, 10)) (makeColorI 0 0 0 0) steps store renderGame eventHandlerEval gameStepEval
                                                      return store
                                                   
evalGameCommand (StateTransformer func) store     = return(updateStateInStore store (func (getStateFromStore store)))
evalGameCommand (SetDirection "N") store          = evalGameCommand(StateTransformer (setDirection N)) store
evalGameCommand (SetDirection "E") store          = evalGameCommand(StateTransformer (setDirection E)) store
evalGameCommand (SetDirection "S") store          = evalGameCommand(StateTransformer (setDirection S)) store
evalGameCommand (SetDirection "W") store          = evalGameCommand(StateTransformer (setDirection W)) store
evalGameCommand (Bind key bindFunction) store     = addKeyMap key c store
  where (FunVal _ c) = storeLookup bindFunction store                           
evalGameCommand (SetScore scoreExp) store         = do
                                                      (IntVal score) <- evalIntExp scoreExp store
                                                      return(updateStateInStore store (Score score))
evalGameCommand (SetColor r g b a str) store      = do
                                                      (IntVal r') <- evalIntExp r store
                                                      (IntVal g') <- evalIntExp g store
                                                      (IntVal b') <- evalIntExp b store
                                                      (IntVal a') <- evalIntExp a store
                                                      evalGameCommand(StateTransformer(setColor r' g' b' a' str)) store
evalGameCommand (AddTo "GOOD" coordExp) store     = do
                                                      coord <- evalCoordExp coordExp store
                                                      evalGameCommand(StateTransformer(\(Playing player lst1 lst2 dir rnd step keyMap colors) -> Playing player (coord:lst1) lst2 dir rnd step keyMap colors)) store
evalGameCommand (AddTo "BAD" coordExp) store      = do
                                                      coord <- evalCoordExp coordExp store
                                                      evalGameCommand(StateTransformer(\(Playing player lst1 lst2 dir rnd step keyMap colors) -> Playing player lst1 (coord:lst2) dir rnd step keyMap colors)) store
evalGameCommand (GoodBad "GOOD" tr) store         = evalGameCommand(StateTransformer(\(Playing player lst1 lst2 dir rnd step keyMap colors) -> Playing player (tr lst1) lst2 dir rnd step keyMap colors)) store
evalGameCommand (GoodBad "BAD" tr)  store         = evalGameCommand(StateTransformer(\(Playing player lst1 lst2 dir rnd step keyMap colors) -> Playing player lst1 (tr lst2) dir rnd step keyMap colors)) store
evalGameCommand (AddRandomBadToRow e) store       = do
                                                      let (Playing player lst1 lst2 dir rnd step keyMap colors) = getStateFromStore store
                                                      (IntVal y) <- evalIntExp e store
                                                      let (x, rnd') = getRandomX rnd
                                                      return(updateStateInStore store (Playing player lst1 ((x, y):lst2) dir rnd' step keyMap colors))

evalGameCommand (ForEach "BAD" func) store        = do
                                                      let (Playing _ _ lst2 _ _ _ _ _) = getStateFromStore store
                                                      evalForEach c lst2 store
  where (FunVal _ c) = storeLookup func store
evalGameCommand (ForEach "GOOD" func) store        = do
                                                      let (Playing _ lst1 _ _ _ _ _ _) = getStateFromStore store
                                                      evalForEach c lst1 store
  where (FunVal _ c) = storeLookup func store
evalForEach :: Command -> [Coord] -> Store -> IO Store
evalForEach c (z:zs) store = do
                               let(x, y) = z
                               let store' = Map.insert "x" (IntVal x) store
                               let store'' = Map.insert "y" (IntVal y) store'
                               store''' <- evalCommand c store''
                               evalForEach c zs store'''
evalForEach c [] store     = return store


-- parse a game-specific command
parseGameCommand:: Parser GameCommand
parseGameCommand = parseCreate `mplus` parseGameLoop `mplus` parseSetDirection `mplus` parseMovePlayerInDir `mplus` parseBind
                   `mplus` parsePlaceRandom `mplus` parseClearGood `mplus` parseClearBad `mplus` parseAddPlayerToGood `mplus` parseAddPlayerToBad
                   `mplus` parseMoveGoodInDir `mplus` parseMoveBadInDir `mplus` parseRemoveLast `mplus` parseRemoveDuplicates
                   `mplus` parseShowScoreScreen `mplus` resetGame `mplus` parseSetColorRGBA `mplus` parseSetColorRGB
                   `mplus` parseAddTo `mplus` parseRemoveGoodBadOverlap `mplus` parseAddRandomBadToRow `mplus` parseBoundPlayerToMap
                   `mplus` parseRemoveOfMap `mplus` parseForEach

  where
  parseCreate               = do
                                stepFunction <- parseVarFunction "createGame"
                                return (CreateGame stepFunction)
  parseGameLoop             = do
                                steps <- parseIntFunction "startGame"
                                return (StartGame steps)
  parseSetDirection         = do
                                dir <- parseVarFunction "setDirection"
                                return (SetDirection dir)
  parseMovePlayerInDir      = do
                                match "moveInDirection(PLAYER)"
                                return(StateTransformer movePlayerInDirection)
  parseMoveGoodInDir        = do
                                match "moveInDirection(GOOD)"
                                return(StateTransformer (\(Playing player lst1 lst2 dir rnd step keyMap colors) -> Playing player (map (`moveCoordInDirection` dir) lst1) lst2 dir rnd step keyMap colors))
  parseMoveBadInDir         = do
                                 match "moveInDirection(BAD)"
                                 return(StateTransformer (\(Playing player lst1 lst2 dir rnd step keyMap colors) -> Playing player lst1 (map (`moveCoordInDirection` dir) lst2) dir rnd step keyMap colors))
  parseBind                 = do
                                match "bind("
                                key <- parseVar
                                cToken ','
                                ignoredTokenMatcher
                                bindFunction <- parseVar
                                cToken ')'
                                return (Bind key bindFunction)
  parsePlaceRandom          = do
                                match "placeRandom(GOOD)"
                                return(StateTransformer placeRandomGood)
  parseClearGood            = do
                                match "clear(GOOD)"
                                return(StateTransformer clearGood)                        
  parseClearBad             = do
                                match "clear(BAD)"
                                return(StateTransformer clearBad)
  parseAddPlayerToGood      = do
                                match "addTo(GOOD, PLAYER)"
                                return(StateTransformer (\(Playing player lst1 lst2 dir rnd step keyMap colors) -> Playing player (player:lst1) lst2 dir rnd step keyMap colors))
  parseAddPlayerToBad       = do
                                match "addTo(BAD, PLAYER)"
                                return(StateTransformer (\(Playing player lst1 lst2 dir rnd step keyMap colors) -> Playing player lst1 (player:lst2) dir rnd step keyMap colors))
  parseRemoveGoodBadOverlap = do
                                match "removeGoodBadOverlap"
                                return(StateTransformer (\(Playing player lst1 lst2 dir rnd step keyMap colors) -> Playing player (lst1\\lst2) (lst2\\lst1) dir rnd step keyMap colors))
  parseAddTo                = do
                                match "addTo("
                                who <- parseVar
                                cToken ','
                                cExp <- parseCoord
                                cToken ')'
                                return(AddTo who cExp)

  parseRemoveLast           = do
                                who <- parseVarFunction "removeLast"
                                return(GoodBad who init)
                                   
  parseShowScoreScreen      = do
                                score <- parseIntFunction "showScoreScreen"
                                return(SetScore score)
                                   
  resetGame                 = do
                                match "resetGame"
                                return(StateTransformer (\(Playing _ _ _ dir rnd step keyMap colors) -> Playing playerStartPosition [] [] dir rnd step keyMap colors))

  parseSetColorRGBA         = do
                                match "setColor("
                                who <- parseVar
                                cToken ','
                                r <- parseIntExp
                                cToken ','
                                g <- parseIntExp
                                cToken ','
                                b <- parseIntExp
                                cToken ','
                                a <- parseIntExp
                                cToken ')'
                                return (SetColor r g b a who)

  parseSetColorRGB          = do
                                match "setColor("
                                who <- parseVar
                                cToken ','
                                r <- parseIntExp
                                cToken ','
                                g <- parseIntExp
                                cToken ','
                                b <- parseIntExp
                                cToken ')'
                                return (SetColor r g b (Lit 255) who)
                                     
  parseRemoveDuplicates     = do
                                match "removeDuplicates("
                                who <- parseVar
                                cToken ')'
                                return(GoodBad who removeDuplicates)

  parseAddRandomBadToRow    = do
                                y <- parseIntFunction "addRandomBadToRow"
                                return(AddRandomBadToRow y)

  parseBoundPlayerToMap     = do
                                match "boundPlayerToMap"
                                return(StateTransformer (\(Playing player lst1 lst2 dir rnd step keyMap colors) -> Playing (boundPlayer player) lst1 lst2 dir rnd step keyMap colors))

  parseRemoveOfMap          = do
                                who <- parseVarFunction "removeOfMap"
                                return(GoodBad who (filter isOnMap))

  parseForEach              = do
                                match "forEach("
                                who <- parseVar
                                cToken ','
                                ignoredTokenMatcher
                                func <- parseVar
                                cToken ')'
                                return(ForEach who func)


------------------------------
-----Engine Expressions-------
------------------------------           

-- parse a game-specific expression
parseGameExpression :: Parser GameExpression
parseGameExpression = parseAmountOfGood `mplus` parseAmountOfBad `mplus` parsePlayerOverlapWithGood `mplus` parsePlayerOverlapWithBad `mplus` parsePlayerOnMap
                      `mplus` parseGetPlayerX `mplus` parseGetPlayerY `mplus` parseGetMaxX `mplus` parseGetMaxY

  where
  parseAmountOfGood          = do
                                 match "amountOf(GOOD)"
                                 return(StateExpression (\(Playing player lst1 _ _ _ _ _ _) -> length lst1))
  parseAmountOfBad           = do
                                 match "amountOf(BAD)"
                                 return(StateExpression (\(Playing player _ lst2 _ _ _ _ _) -> length lst2))
  parsePlayerOverlapWithGood = do
                                 match "playerOverlapWith(GOOD)"
                                 return(StateExpression (\(Playing player lst1 _ _ _ _ _ _) -> fromEnum (hasOverlapping [player] lst1)))
  parsePlayerOverlapWithBad  = do
                                 match "playerOverlapWith(BAD)"
                                 return(StateExpression (\(Playing player _ lst2 _ _ _ _ _) -> fromEnum (hasOverlapping [player] lst2)))
  parsePlayerOnMap           = do
                                 match "playerOnMap"
                                 return(StateExpression (\(Playing player _ _ _ _ _ _ _) -> fromEnum (isOnMap player)))
  parseGetPlayerX            = do
                                 match "getPlayerX"
                                 return(StateExpression (\(Playing (x, _) _ _ _ _ _ _ _) -> x))
  parseGetPlayerY            = do
                                 match "getPlayerY"
                                 return(StateExpression (\(Playing (_, y) _ _ _ _ _ _ _) -> y))
  parseGetMaxX               = do
                                 match "getMaxX"
                                 return (GameLit xsize) 
  parseGetMaxY               = do
                                 match "getMaxY"
                                 return (GameLit ysize) 


-- evaluate a game-specific expression
evalGameExpression :: GameExpression -> Store -> IO Int
evalGameExpression (StateExpression stateExp) store = do
                                                        let state = getStateFromStore store
                                                        return(runStateExp stateExp state)
evalGameExpression (GameLit i) store = return i

-- run an expression that changes the GameState
runStateExp :: (GameState -> Int) -> GameState -> Int
runStateExp stateExp (Score _) = 0
runStateExp stateExp state     = stateExp state

-- parse a coordinate
parseCoord :: Parser CoordExp
parseCoord = do
               e <- parseIntExp;
               cToken ',';
               f <- parseIntExp;
               return (e, f)
               
-- evaluate a coordinate
evalCoordExp :: CoordExp -> Store -> IO Coord
evalCoordExp (e, f) store = do
                              (IntVal e') <- evalIntExp e store
                              (IntVal f') <- evalIntExp f store
                              return (e', f')