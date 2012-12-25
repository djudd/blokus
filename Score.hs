module Score (
  heuristicScores,
  finalScores,
  winner
) where

import Data.List

import Types
import Offset
import Player

heuristicScores :: GameState -> [Double]
heuristicScores node = finalScores node 

winner :: GameState -> Player
winner node = 
  let scores = finalScores node
   in case find (\p -> (scores !! (getIndex p)) == 1.0) allPlayers of
      (Just p) -> p
      Nothing -> error "Couldn't pick a winner! Even in the case of a tie we should just take the first..."

finalScores :: GameState -> [Double]
finalScores node = 
  let points = currentPoints node
      winning = maximum points
   in [(if p == winning then 1.0 else 0.0) | p <- points] 

currentPoints :: GameState -> [Int]
currentPoints (State _ _ _ pieces) = map' (sum . map getValue) pieces
