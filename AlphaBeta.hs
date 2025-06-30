module AlphaBeta where 

import Data.Ord(comparing)
import Data.List(sortBy)
import Numeric.IEEE(infinity)
import Data.Maybe(fromMaybe)
import Debug.Trace(trace)
import Game 

-- Alpha/Beta Search with depth bound
alphabeta :: Game b a => Int-> b -> ([a], Double)
alphabeta bound b = 
  let ms = maximumsBy (comparing snd)
                      [(a, minValue bound (result b a) Nothing) | a<- actions b ]
  in  (map fst ms, snd (head ms))

maxValue :: Game b a => Int-> b -> Maybe Double-> Double
maxValue d s malpha
  | terminal s || d== 0 = utility s
  | otherwise  = maxs (actions s) (-infinity) where
      maxs [] m = m 
      maxs (a:as) m
        | v >= alpha = v 
        | otherwise  = maxs as (max v m)
          where v = minValue (d-1) (result s a) (Just m)
      alpha = fromMaybe infinity malpha 

minValue :: Game b a => Int-> b-> Maybe Double-> Double
minValue d s mbeta
  | terminal s || d == 0 = utility s
  | otherwise  = mins (actions s) infinity where 
      mins [] m = m 
      mins (a:as) m 
        | v <= beta = v 
        | otherwise = mins as (min v m) 
          where v = maxValue (d-1) (result s a) (Just m) 
      beta = fromMaybe (-infinity) mbeta 

-- Utility functions

-- Like minimumBy, but returns *all* mininal elements
minimumsBy :: (a-> a-> Ordering )-> [a]-> [a]
minimumsBy _ [] = [] -- should not happen, nevermind
minimumsBy cmp xs =
  takeWhile (\y-> cmp (head x) y == EQ) x where x = sortBy cmp xs

-- Like mininumsBy, but returns all *largest* elements
maximumsBy :: (a-> a-> Ordering )-> [a]-> [a]
maximumsBy cmp = minimumsBy (flip cmp) 
