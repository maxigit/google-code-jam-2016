
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import Data.List.Split
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
  getLine
  bff <- map read . words <$> getLine

  let types = bff :: [Int]

  let m = toMap bff
  print m
  let pathMap = map (pathFrom m) [1 ..length bff]
  print pathMap


f = undefined

data Path
  = Pair [Int]
  | Loop [Int] -- ex 1 2 3 1
  | ToPair [Int] -- ex 1 2 3 2
  | ToLoop [Int] 
  deriving Show

toMap :: [Int] -> Map Int Int
toMap xs = Map.fromList $ zip [1..] xs
  

          
next m i = fromJust $ Map.lookup i m
-- computes the path starting from the given point
path m i0 (vs) prev i | i `elem` vs = (i0, (i:vs), prev, i)
                 | otherwise = path m i0 (i:vs) i (next m i)


pathFrom m i = let 
  (start, visited, ante, end) = path m i [] i i
  in case visited of
	    [a,b,c] | a == c  -> Pair visited
	    (a:b:c:_) | a == c  -> ToPair visited
	    _ | start == end -> Loop visited
	    _ -> ToLoop visited
  
