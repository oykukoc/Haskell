{-# LANGUAGE FunctionalDependencies #-}
module Game where

-- A Game: a board with actions  
class Ord a => Game b a | b -> a where
  terminal :: b -> Bool    -- Terminal state, no further actions
  utility  :: b -> Double  -- Pay-off functions (defined at least for terminal states)
  actions  :: b -> [a]     -- All possible actions from here
  result   :: b -> a -> b  -- Result of applying action to a state 

