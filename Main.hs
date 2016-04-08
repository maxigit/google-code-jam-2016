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

main :: IO ()
main = do
	n <- read <$> getLine
	mapM_ (solve f) [1..n]


solve f i = do 
  putStr$ "Case #" ++ show i ++ "# "

  n <- read <$> getLine
  putStrLn $ show (n :: Int)


f :: Int -> String
f n = go n (Set.empty) 1 where
  go _ _ 10 = "INSOMNIA"
  go n done i | length (Set.toList done) == 10 = show $ i-1
  go n done i = let numbers = show (n*i)
     in go n (done <> Set.fromList numbers) (i+1)
  


	
