module Main where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Ord
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
 -- putStrLn (show (k, c, s))
 let originalss = map (pattern c) (diag k)
     ss = go originalss
     go pat = case pick pat of
                   Nothing ->  []
                   Just (col, leftover) -> col : go leftover

 --putStrLn $ show originalss
 --putStrLn $ show ss
 if (length ss > s) 
 then putStrLn "IMPOSSIBLE"
 else putStrLn (intercalate " " $ map (show . (+1)) ss)

diag :: Int -> [[Tile]]
diag k = [ replicate i L ++ [G] ++ replicate (k-1-i) L | i <- [0..(k-1)]]

data Tile = L | G deriving (Show, Read, Eq)

nextGeneration :: [Tile] -> [Tile] -> [Tile] -> [Tile]
nextGeneration original golds pat = concatMap next pat
  where next L = original
        next G = golds


pattern :: Int -> [Tile] ->  [Tile]
pattern c originals =  go originals c
  where go pat 1 = pat
        go pat n = go (nextGeneration originals golds pat) (n-1)
        golds = replicate (length originals) G

-- Find for a list of patter the column with the more gold common in each patter
-- return ambigoous pattern

pick :: [[Tile]] -> Maybe (Int, [[Tile]])
pick [] = Nothing
pick pats = let
 l = length (head pats)
 columns = Map.fromListWith (+)
			    [ (i, 1)
                            | i <- [0..(l-1)]
                            , pat <- pats
                            , pat !! i == G
                            ]
 in case sortBy (comparing (Down . snd)) (Map.toList columns) of
   [] -> Nothing
   ((bestColumn, _): _) -> Just (bestColumn, filter (\pat -> pat !! bestColumn /= G) pats)


