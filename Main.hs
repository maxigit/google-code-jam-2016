
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
  [k, c, s] <- map read . words <$> getLine

  f k c s

-- a column is represented as fractal coordinate
--in each index correspond to one generation
--ex 1.2.3 for fist
f :: Int -> Int -> Int -> IO ()
f k c s = do
  -- we compute for each column which initial gold tiles it's supposed to check
  -- each columns can check c initial gold tile at the same time
  -- we need to check k columns and group the indexes by k
  let indexes = chunksOf c [0..(k-1)]

  if (length indexes) > s
  then putStrLn "IMPOSSIBLE"
  else  do -- we need to convert list of index to a column
     let cols = map (show . toColumn k c) indexes
     -- let cols = map (show  ) indexes
     mapM_ putStr (intersperse " " cols)
     putStrLn ""


toColumn :: Int -> Int -> [Int] -> Int
toColumn k c is = let 
  weights = map (k^) [c-1,c-2..0]
  in 1 + (sum $ zipWith (*) is weights)
