module Search (
    minimax
) where

import Data.List

import Types
import GameState

minimax :: Int -> GameState -> [Float]
minimax 0 node      = heuristicScores node
minimax depth node  = case getChildren node of 
    []          -> finalScores node
    children    -> {-# SCC "children" #-} maximumBy comparator (recurse children)
    where recurse = {-# SCC "scoreChildren" #-} map (minimax (depth-1))
          comparator a b = {-# SCC "compareScores" #-} compare (a !! i) (b !! i)
          i = getPlayerIndex node

heuristicScores node = [0.5, 0.5, 0.5, 0.5]

finalScores node = [1.0, 1.0, 1.0, 1.0]
