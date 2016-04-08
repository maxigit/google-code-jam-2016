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
  putStr$ "Case #" ++ show i ++ ": "

  n <- read <$> getLine
  putStrLn (f n)


f :: Int -> String
f n = go n (Set.empty) 0 where
go 0 _ _ = "INSOMNIA"
go n done n' | length (Set.toList done) == 10 = show $ n'
go n done n' = let numbers = show (n'')
                   n'' = n + n'
  in go n (done <> Set.fromList numbers) (n'')
  
  


	
