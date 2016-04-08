module Main where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import Data.Map

main :: IO ()
main = do
	n <- read <$> getLine
	mapM_ (solve f) [1..n]


solve f i = do 
  putStr$ "Case #" ++ show i ++ "# "
  n <- read <$> getLine
  putStrLn $ show (n :: Int)

f :: Int -> String
f _ = "INSOMNIA"
  


	
