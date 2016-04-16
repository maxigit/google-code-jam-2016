
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
	-- read the number of cases
	n <- read <$> getLine
	mapM_ (solve maximumCircle ) [1..n]


solve f i = do 
  putStr$ "Case #" ++ show i ++ ": "
  getLine -- we don't need the number of elements
  bff <- map read . words <$> getLine

  let m = toMap bff
  -- computes for each children, the chain of BFF needed
  -- this part could be optimised because if 1 bff is 2,
  -- then we can reuse pathFrom 2 without having to recalculate it.
  let pathMap = map (pathFrom m) [1 ..length bff]


  -- print pathMap
  -- solution to problem
  print $ f pathMap

-- Computes the maximum circles so that each children
-- is near to its best friends.
-- The basic idea is that if there are cycles in the graph of friends
-- only ONE can be used to form a circle.
-- so we obvioulsly look at the biggest one.
-- However, pair of BFF (1 -> 2 and 2 -> 1) are self sufficient
-- and can be but next to each other, regardless of their neighbour.
-- Example:
--     if 1 <-> 2 and 3<-> 4. we can do (1 2) (3 4) circle and if fact
-- doesn't even need to care about the fact it's a circle.
--  More interstingly, sequence ofs BFF ending to a pair can also be treated in isolation
--  Example: 
-- for the given input 2 3 4 3 4 7 6 3
-- 1 -> 2 -> (3 <-> 4)
-- 5 -> (4 <-> 3)
-- 6 <-> 7
-- 8 -> (3 -> 4)
-- the two first chains end up to a pair so can be put next to any other sequence.
-- We can happyly make the following circle
-- (1 -> 2 -> (3 <-> 4) <- 5) (6 <-> 7).
-- 8 unfortunatly can't join the circle.
-- The problem is then for each pair to find the longuest chain on both side and add everything up.

maximumCircle :: [Path] -> Int
maximumCircle p = max cycles fromChain where
 loops = filter isLoop p
 cycles = maximum' (map pathLength loops)
 -- we need to find for each element of a pair the longest chain ending to it.
 -- For that, we group all chains by their termination point.
 chains = Map.toList $ Map.fromListWith (<>) [(head l, [path] ) | path <- p, let l = pa path, isPair path]
 -- In our example chain we will have
 -- 1 2 3 4
 -- 5 4 3
 -- 6 7
 -- 6 7
 -- Which be grouped as
 -- 4 : (1 2 3 4), (8 3 4)
 -- 3 : 5 4
 -- 7 : 6 7
 -- 6 : 6 7
 -- The chain (1 2 3 4) is longuer that (8 3 4) so we take it.
 -- Each pair is in fact seen twice depending on which side we arrive.
 fromChain = sum' (map longuest chains) - length chains -- to count pair only once
 -- we then have to sum : 4 + 2 + 2 + 2 = 10 
 -- and remove 2 from each pair (of 1 for each sequence)
 -- we get 10 - (2*2) = 6
 -- which is indeed the lenght of (1 -> 2 -> (3 <-> 4) <- 5) (6 <-> 7).


-- probaly not need but we never know.
sum' [] = 0
sum' xs = sum xs
maximum' [] = 0
maximum' xs = maximum xs
-- get the two longuest chains with nothing common
-- To find
longuest (_, []) = 0
longuest (_, cs) = maximum $ map pathLength cs 
 



-- | A chain of  Best friend forever.
--  which can be a pair a true loop
-- or pass chain connecting to a pair or a loop.
data Path
  = Pair [Int]
  | Loop [Int] -- ex 1 2 3 1
  | ToPair [Int] -- ex 1 2 3 2
  | ToLoop [Int] 
  deriving Show


pathLength (Pair ps ) = length ps - 1
pathLength (ToPair ps ) = length ps - 1
pathLength (ToLoop ps ) = length ps - 1
pathLength (Loop ps ) = length ps - 1

pa (Pair ps ) = ps
pa (ToPair ps ) = ps
pa (ToLoop ps ) = ps
pa (Loop ps ) = ps

isLoop (Loop _) = True
isLoop _ = False

isPair (Pair ps) = True
isPair (ToPair ps) = True
isPair _ = False

-- | Create a map of BFF from a list of Inte
-- assuming that list index start at 1.
-- example [2 1] mean the best friend of 1 is 2 and vice versa
toMap :: [Int] -> Map Int Int
toMap xs = Map.fromList $ zip [1..] xs
  

          

-- | Finds the best friend from a map
next :: Map Int Int -> Int -> Int
-- Ugly, but we know that the input is well form ;-)
next m i = fromJust $ Map.lookup i m
-- computes the path starting from the given point
path m i0 (vs) prev i | i `elem` vs = (i0, (i:vs), prev, i)
                 | otherwise = path m i0 (i:vs) i (next m i)


-- | The core of the algorithm, detects loops and everyting :-)
pathFrom m i = let 
  (start, visited, ante, end) = path m i [] i i
  in case visited of
	    [a,b,c] | a == c  -> Pair visited
	    (a:b:c:_) | a == c  -> ToPair visited
	    _ | start == end -> Loop visited
	    _ -> ToLoop visited
  
