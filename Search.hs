module Search (
    minimax
) where

import Data.List
import Data.Ord

import Types
import Player
import GameState
import Score

minimax :: Int -> GameState -> [Double]
minimax 0 node      = heuristicScores node
minimax depth node  = case getChildren node of 
    []          -> finalScores node
    children    -> {-# SCC "children" #-} maximumBy comparator (recurse children)
    where recurse = {-# SCC "scoreChildren" #-} map (minimax (depth-1))
          comparator = {-# SCC "compareScores" #-} comparing (!! i)
          i = getIndex (getPlayer node)
