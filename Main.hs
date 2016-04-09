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
	mapM_ (solve ) [1..n]


solve i = do 
  putStr$ "Case #" ++ show i ++ ": "


  putStrLn ""
  [n, j] <- map read . words <$> getLine
  mapM_ printJam (jamcoins n j)


jamcoins :: Int -> Int -> [(Int, [Int])]
jamcoins n j = take j coins
  where coins = [ (i,  divs)
                | i <- generateN n
                , let divM = map divider (map (nToBase i) [2..10])
		, divs <- maybeToList $ sequence divM
                ]

printJam (jam, divs) = do
 -- proof
 let ns = map (nToBase jam)  [2..10]
     proof n d = do
         let d' = n `div` d
         print $ show n ++ " = " ++ show d ++ "*" ++ show d'
 -- zipWithM_ proof ns divs
 putStrLn (intercalate " " (map show (nToBase jam 10 :divs)))
 


generateN :: Int -> [Int]
generateN' n = [0..n'-1] where n' = 2 ^ n
-- fist and last number are one, so we don't need to generate them
generateN n = map (\i -> 1+ 2*i + 2^(n-1)) (generateN' (n-2))

-- Generate number b (as bits) in it's equivalent in base b
nToBase 1 _ = 1
nToBase 0 _ = 0
nToBase n base = let r = n `mod` 2
      in r + base * (nToBase (n `div` 2) base)
 

-- find first dividers
divider :: Int -> Maybe Int
divider n = listToMaybe $  filter (\i -> n `mod` i == 0) (takeWhile (\i -> i*i <= n) [2..n-1])

