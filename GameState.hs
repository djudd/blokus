module GameState (

) where

import Data.Bits

import Types
import Board
import Territory
import Placement

instance Show GameState where
    show (State _ board _ _) = showBoard board

newGame = State 0 emptyBoard initialCorners initialPlacements

currentPlayer :: Turn -> Player
currentPlayer turn = (turn `mod` 4) + 1

getChild (State turn board corners placements) (TerritoryCorner x y direction _) placement =
    let player = currentPlayer turn
        move = Move player x y placement
        turn' = turn + 1
        board' = getBoardAfterMove board move
        corners' = getCornersAfterMove board' move corners
        placements' = getPlacementsAfterMove move placements
     in State turn' board' corners' placements'

legalAt (TerritoryCorner _ _ _ cornerBitmap) (Placement _ _ _ placementBitmap) = (placementBitmap .&. cornerBitmap) == placementBitmap

getChildren (State turn board corners placements) =
    let getMyChild = getChild (State turn board corners placements)
        mover = currentPlayer turn
        moverCorners = corners !! (fromIntegral mover)
        moverPlacements = placements !! (fromIntegral mover)
     in [getMyChild corner placement | corner <- moverCorners, placement <- moverPlacements, legalAt corner placement]

main = print $ getChildren newGame
