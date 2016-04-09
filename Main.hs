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
	mapM_ (solve f) [1..n]


solve f i = do 
  putStr$ "Case #" ++ show i ++ ": "

  pancakes <- getLine
  putStrLn $ show (f pancakes)


-- we can ignore the  happy pancackes at the bottom
f p = go  (reverse p) 0
go [] n = n
go ('+':ps) n  = go ps n
-- if we have unhappy on the top we flip everything
go ps n = let r = case split ps of
			    (bot, mid, []) -> go (turn (bot ++ mid)) (n+1)
			    (bot, mid, top) ->  go (bot ++ turn (mid ++ top)) (n+1)
    in trace (show $ ((ps, n),  split ps)) r



inverse '+' = '-'
inverse '-' = '+'

turn = reverse . map inverse

-- split a pile in unhappy at the bottom and happy at the top
split :: [Char] -> ([Char], [Char], [Char])
split [] = ([], [], [])
split ('-':ps) = let (unhappys, mid, happys) = split ps
  in ('-':unhappys, mid, happys)

-- find the bottom, 
split  (unhappy:ps) = case split (turn ps) of  
   (happys, [], []) -> ([], [],  unhappy : turn happys)
   (happys, mid', unhappys) -> ([], unhappy :  turn (mid'++unhappys), turn happys)
     
  
  


	
