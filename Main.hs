module Main where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set  as Set
import Debug.Trace

main :: IO ()
main = do
	n <- read <$> getLine
	mapM_ (solve f ) [1..n]


solve f i = do 
  putStr$ "Case #" ++ show i ++ ": "
  [k, c, s] <- map read . words <$> getLine

  f k c s

f :: Int -> Int -> Int -> IO ()
f k c s = do
 putStrLn (show (k, c, s))
 

data Tile = L | G deriving (Show, Read, Eq)

nextGeneration :: [Tile] -> [Tile] -> [Tile] -> [Tile]
nextGeneration original golds pat = concatMap next pat
  where next L = original
        next G = golds


pattern :: [Tile] -> Int -> [Tile]
pattern originals c =  go originals c
  where go pat 1 = pat
        go pat n = go (nextGeneration originals golds pat) (n-1)
        golds = replicate (length originals) G
