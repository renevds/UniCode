import Interpreter
import Parser
import Store
import System.Environment (getArgs)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO

main :: IO ()
main = do (filename:_) <- getArgs -- the first argument
          inputHandle <- openFile filename ReadMode 
          hSetEncoding inputHandle utf8 -- set encoding to handle unicode characters
          contents <- hGetContents inputHandle
          _ <- evalCommand (parse parseCommand contents) Map.empty -- returns a store with variables at the end
          return()