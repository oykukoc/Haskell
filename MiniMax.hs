module MiniMax(minimax,maximumsBy,minimumsBy) where 

import Data.Ord(comparing)
import Data.List(sortBy)
import Game 

-- Minmax with a depth bound
minimax :: Game b a => Int-> b -> ([a], Double)
minimax bound b = 
  let ms = maximumsBy (comparing snd)
                      [(a, minValue bound (result b a)) | a<- actions b ]
  in  (map fst ms, snd (head ms))

maxValue :: Game b a => Int-> b -> Double
maxValue d s 
  | terminal s || d== 0 = utility s
  | otherwise  = maximum [ minValue (d-1) (result s a)| a <- actions s]

minValue :: Game b a => Int-> b-> Double
minValue d s 
  | terminal s || d == 0 = utility s
  | otherwise  = minimum [ maxValue (d-1) (result s a) | a <- actions s ]

-- Utility functions

-- Like minimumBy, but returns *all* mininal elements
minimumsBy :: (a-> a-> Ordering )-> [a]-> [a]
minimumsBy _ [] = [] -- should not happen, nevermind
minimumsBy cmp xs =
  takeWhile (\y-> cmp (head x) y == EQ) x where x = sortBy cmp xs

-- Like mininumsBy, but returns all *largest* elements
maximumsBy :: (a-> a-> Ordering )-> [a]-> [a]
maximumsBy cmp = minimumsBy (flip cmp) 
