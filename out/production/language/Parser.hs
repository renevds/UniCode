module Parser where

import Data.Char
import Control.Monad
import Control.Applicative

-- tokens to be ignored while parsing
ignoredTokens :: [Char]
ignoredTokens = ['\n', ' ', '\r']

newtype Parser a = Parser (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (Parser f)  =  f

-- return parsed value
parse :: Parser a -> String -> a
parse m s  =  one [ x | (x,t) <- apply m s, t == "" ]
  where
  one []                  =  error "Could not parse the file"
  one [x]                 =  x
  one xs | length xs > 1  =  error "ambiguous parse"


instance Functor Parser where
  fmap = liftM

instance Applicative Parser where 
   pure  = return 
   (<*>) = ap 

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

instance Monad Parser where
  return x  =  Parser (\s -> [(x,s)])
  m >>= k   =  Parser (\s ->
                 [ (y, u) |
                   (x, t) <- apply m s,
                   (y, u) <- apply (k x) t ])

instance MonadPlus Parser where
  mzero      =  Parser (const [])
  mplus m n  =  Parser (\s -> apply m s ++ apply n s)

-- parse one char
char :: Parser Char
char =  Parser f
  where
  f []     =  []
  f (c:s)  =  [(c,s)]

-- parse char satisfying predicate
spot :: (Char -> Bool) -> Parser Char
spot p  =  do
             c <- char
             guard (p c)
             return c

-- match a char
token :: Char -> Parser Char
token c  =  spot (== c)

-- match one of list of chars
tokens :: [Char] -> Parser Char
tokens c = spot (`elem` c)

-- match a given string
match :: String -> Parser String
match []      =  return []
match (x:xs)  =  do
                   y <- token x
                   ys <- match xs
                   return (y:ys)

-- match zero or more occurrences
star :: Parser a -> Parser [a]
star p  =  plus p `mplus` return []

-- match one or more occurrences
plus :: Parser a -> Parser [a]
plus p  =  do 
             x <- p
             xs <- star p
             return (x:xs)

-- match a natural number
parseNat :: Parser Int
parseNat =  do
              s <- plus (spot isDigit)
              return (read s)

-- match a negative number
parseNeg :: Parser Int
parseNeg =  do
              token '-'
              n <- parseNat
              return (-n)

-- match an integer
parseInt :: Parser Int
parseInt =  parseNat `mplus` parseNeg

-- match a string
parseVar :: Parser String
parseVar =  do
              plus (spot isLetter)
               
-- match ignored tokens
ignoredTokenMatcher :: Parser Char
ignoredTokenMatcher = tokens ignoredTokens

-- match any amount of ignored tokens
ignoredTokenCleaner :: Parser String
ignoredTokenCleaner = star ignoredTokenMatcher

-- match token/string with leading ignored characters
cToken :: Char -> Parser Char
cToken c = do
             ignoredTokenCleaner
             token c
cMatch :: String -> Parser String
cMatch c = do
             ignoredTokenCleaner
             match c